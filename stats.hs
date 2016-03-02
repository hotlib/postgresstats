{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Data.Time.Clock (UTCTime)
import Text.PrettyPrint.ANSI.Leijen
import System.Console.GetOpt
import System.Environment
import Control.Monad.Reader 

type TableSizeAndWritesSelectResult = (String, String, Maybe Int)
type LeastUsedIndexesResult = (String, Int, Int)
type SharredBufferTableResult = (String, Int)
type SharredBufferOverallResult = (Maybe Int, Maybe Int, Maybe Double)
type UnusedIndexesResult = (String, String, Int, String, String, Maybe Int)
type LastAnalyzedAndVacuumedResult = (String, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime)
type TransactionStuckResult = (Bool, Maybe UTCTime, String, Maybe UTCTime, Maybe UTCTime)
type FindLongestTransactionResult = (Only Double)
type FindLongestQueryResult = (Double, String)
type MostAccessedTableResult = (String, Int, Maybe Int, Maybe Int)
type BiggestTableResult = (String, String)

type PgConnection r = ReaderT ConnectInfo IO r 

data PgArguments = Ip String | DbName String | Username String | Password String


biggestTableText :: Doc
biggestTableText = text "(table, size)"

biggestTableSelect :: Query
biggestTableSelect = "SELECT \
		\table_schema || '.' ||table_name AS schema_table, \
		\pg_size_pretty(pg_relation_size(table_name)) as table_size \
		\FROM information_schema.tables WHERE table_schema NOT IN ('pg_catalog', 'information_schema') \
		\ORDER BY table_size DESC LIMIT 5"

mostAccessedTableText :: Doc
mostAccessedTableText = text "(table, index + sequential scans, % of index hits, % of write accesses)"

mostAccessedTableSelect :: Query
mostAccessedTableSelect = "SELECT \
	\schemaname || '.' || relname AS schema_table, \
	\COALESCE(idx_scan + seq_scan, seq_scan, idx_scan) as scans, \
	\100 * idx_scan / NULLIF(COALESCE(idx_scan + seq_scan, seq_scan,idx_scan), 0) AS index_hit_avg, \
	\100 * COALESCE( \
	\n_tup_ins + n_tup_del + n_tup_upd, \
	\n_tup_ins + n_tup_del, \
	\n_tup_del + n_tup_upd, \
	\n_tup_upd + n_tup_ins, \
	\n_tup_ins, n_tup_upd, n_tup_del)/ \
	\NULLIF(( \
	\COALESCE(idx_scan + seq_scan,seq_scan,idx_scan) + \
	\COALESCE( \
	\n_tup_ins + n_tup_del + n_tup_upd, \
	\n_tup_ins + n_tup_del, \
	\n_tup_del + n_tup_upd, \
	\n_tup_upd + n_tup_ins, \
	\n_tup_ins, n_tup_upd, n_tup_del)), 0) AS write_percentage \
	\FROM pg_stat_user_tables WHERE \
	\(idx_scan IS NOT NULL OR seq_scan IS NOT NULL) \
	\ORDER BY scans DESC LIMIT 5"


findLongestQueryText :: Doc
findLongestQueryText = text "(query time in seconds, query)"

findLongestQuerySelect :: Query
findLongestQuerySelect = "SELECT \
	\COALESCE(extract(epoch FROM CURRENT_TIMESTAMP-query_start),0) AS query_time_in_seconds, \
	\state FROM pg_stat_activity WHERE state NOT LIKE 'idle%' \
	\GROUP BY query_time_in_seconds, state \
	\ORDER BY query_time_in_seconds DESC \
	\LIMIT 3"

findLongestTransactionText :: Doc
findLongestTransactionText = text "(transaction time in seconds)"

findLongestTransactionSelect :: Query
findLongestTransactionSelect = "SELECT \
	\COALESCE(max(extract(epoch FROM CURRENT_TIMESTAMP-xact_start)),0) AS transaction_time_in_seconds \
	\FROM pg_stat_activity \
	\ORDER BY transaction_time_in_seconds DESC \
	\LIMIT 3"

transactionStuckText :: Doc
transactionStuckText = text "(is transaction waiting, start of transaction, current query in transaction, start of query, start of connection)"

transactionStuckSelect :: Query
transactionStuckSelect = "SELECT \
	\waiting, \
	\xact_start, \
	\state, \ 
	\query_start, \
	\backend_start \
	\FROM pg_stat_activity WHERE \
	\(state = 'idle in transaction' OR waiting = TRUE) \
	\AND (xact_start < (now()::DATE - interval '20 minute')::TIMESTAMP)"

lastAnalyzedAndVacuumedText :: Doc
lastAnalyzedAndVacuumedText = text "(table, last vaccumed, last auto-vacuumed, last analyzed, last auto-analyzed)"

lastAnalyzedAndVacuumedSelect :: Query
lastAnalyzedAndVacuumedSelect = "SELECT \
	\schemaname || '.' || relname AS schema_table, \
	\last_vacuum, \
	\last_autovacuum, \
	\last_analyze, \
	\last_autoanalyze \
	\FROM pg_stat_all_tables \
	\WHERE NOT \
	\((last_vacuum > (now()::DATE - 14)::TIMESTAMP OR last_autovacuum > (now()::DATE - 14)::TIMESTAMP) \
	\AND (last_analyze > (now()::DATE - 14)::TIMESTAMP OR last_autoanalyze > (now()::DATE - 14)::TIMESTAMP)) \
	\OR (last_autovacuum IS NULL AND last_vacuum IS NULL) \
	\OR (last_analyze IS NULL AND last_autoanalyze IS NULL)"

unusedIndexesText :: Doc
unusedIndexesText = text "(table, index name, index scans, size of table, size of index, % of read accesses)"

unusedIndexesSelect :: Query
unusedIndexesSelect = "SELECT \
	\pg_index.schemaname || '.' || pg_index.relname AS schema_table, \
	\indexrelname AS index_name, \
	\pg_index.idx_scan AS index_scan_count, \
	\pg_size_pretty(pg_total_relation_size(pg_index.schemaname || '.' || pg_index.relname)) AS table_size, \
	\pg_size_pretty(pg_relation_size(quote_ident(indexrelname)::text)) AS index_size, \
	\100 * COALESCE(pg_tables.idx_scan + seq_scan,seq_scan, pg_tables.idx_scan)/ \
	\NULLIF(( \
	\COALESCE(pg_tables.idx_scan + seq_scan, seq_scan, pg_tables.idx_scan) + \
	\COALESCE( \
	\n_tup_ins + n_tup_del + n_tup_upd, \
	\n_tup_ins + n_tup_del, \
	\n_tup_del + n_tup_upd, \
	\n_tup_upd + n_tup_ins, \
	\n_tup_ins, n_tup_upd, n_tup_del)),0) AS read_percentage \
	\FROM pg_stat_user_indexes AS pg_index \
	\JOIN pg_indexes ON (indexrelname = indexname AND pg_index.schemaname = pg_indexes.schemaname) \
	\JOIN pg_stat_user_tables AS pg_tables ON (pg_index.relname = pg_tables.relname AND pg_index.schemaname = pg_tables.schemaname) \
	\WHERE indexdef !~* 'unique' \
	\ORDER BY index_scan_count ASC \
	\LIMIT 5"

sharredBufferOverallText :: Doc
sharredBufferOverallText = text "(disk reads for non-indexed data, sharred buffer \
								\hits for non-indexed data, % of total sharred buffer hits for non-indexed data)"

sharredBufferOverallSelect :: Query
sharredBufferOverallSelect = "SELECT \ 
	\sum(heap_blks_read) :: BigInt as heap_read, \
    \sum(heap_blks_hit) :: BigInt as heap_hit, \
    \sum(heap_blks_hit) / (sum(heap_blks_hit) + sum(heap_blks_read)) :: Float as buffer_avg \
    \FROM pg_statio_user_tables \
    \WHERE heap_blks_read > 0"

sharredBufferTableText :: Doc
sharredBufferTableText = text "(table, % of sharred buffer hits)"

sharredBufferTableSelect :: Query
sharredBufferTableSelect = "SELECT \  
	\schemaname || '.' || relname AS schema_table, \ 
	\100 * heap_blks_hit/(heap_blks_hit + heap_blks_read) as cache_hit_avg \
	\FROM pg_statio_user_tables \
	\WHERE \
	\heap_blks_read > 0 \
	\ORDER BY cache_hit_avg ASC \
	\LIMIT 5"

leastUsedIndexesText :: Doc
leastUsedIndexesText = text "(table, % of index scans, rows in table) "

leastUsedIndexesSelect :: Query
leastUsedIndexesSelect = "SELECT \
	\schemaname || '.' || relname AS schema_table, \ 
    \100 * idx_scan / (seq_scan + idx_scan) AS index_hit_avg, \
    \n_live_tup AS rows_in_table \
    \FROM pg_stat_user_tables \
    \WHERE \
    \seq_scan + idx_scan > 0 AND \
    \100 * idx_scan / (seq_scan + idx_scan) < 80 \
    \ORDER BY n_live_tup DESC, index_hit_avg DESC \
    \LIMIT 5" 

tableSizeAndWritesText :: Doc
tableSizeAndWritesText = text "(table, size of table (without indexes), % of write accesses) "

tableSizeAndWritesSelect :: Query
tableSizeAndWritesSelect = "SELECT \
	\nspname || '.' || C.relname AS schema_table, \
	\pg_size_pretty(pg_total_relation_size(C.oid)) AS total_size, \
	\100 * COALESCE( \
	\n_tup_ins + n_tup_del + n_tup_upd, \
	\n_tup_ins + n_tup_del, \
	\n_tup_del + n_tup_upd, \
	\n_tup_upd + n_tup_ins, \
	\n_tup_ins, n_tup_upd, n_tup_del)/ \
	\NULLIF(( \
	\COALESCE(idx_scan + seq_scan,seq_scan,idx_scan) + \
	\COALESCE( \
	\n_tup_ins + n_tup_del + n_tup_upd, \
	\n_tup_ins + n_tup_del, \
	\n_tup_del + n_tup_upd, \
	\n_tup_upd + n_tup_ins, \
	\n_tup_ins, n_tup_upd, n_tup_del)), 0) AS write_percentage \
	\FROM pg_class C \
	\LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace) \
	\LEFT JOIN pg_stat_user_tables S ON (S.relname = C.relname) \
	\WHERE nspname NOT IN ('pg_catalog', 'information_schema') \
	\AND C.relkind <> 'i' \
	\AND nspname !~ '^pg_toast' \
	\AND (n_tup_ins IS NOT NULL OR n_tup_upd IS NOT NULL OR n_tup_del IS NOT NULL) \
	\AND (idx_scan IS NOT NULL OR seq_scan IS NOT NULL) \
	\ORDER BY write_percentage DESC \
	\limit 5"


pgRunQuery :: (FromRow r, Show r) => Query -> PgConnection [r]
pgRunQuery queryString = do 
	connection <- ask
	conn <- (liftIO $ connect connection)
  	liftIO $ query_ conn queryString


pgPrintResult :: (FromRow r, Show r) => [r] -> PgConnection ()
pgPrintResult result = do 
	liftIO $ putDoc $ text "Results:" <+> align (vsep $ decorate result) <> linebreak <> linebreak
	where decorate = fmap (red . text . show)


pgPutDoc :: String -> Doc -> PgConnection ()
pgPutDoc caption doc = liftIO $ putDoc $ (text caption) <+> dullred doc <> linebreak 

		
header progName = "Usage: " ++ progName ++ " [OPTION...]"


options :: [OptDescr PgArguments]
options = [
	Option ['i'] ["ip"]       (ReqArg (\s -> Ip s) "IP_ADDRESS") "IP Address of the PostgreSQL server",
	Option ['d'] ["db"] (ReqArg (\s -> DbName s) "DB_NAME") "Name of Database",
	Option ['u'] ["user"] (ReqArg (\s -> Username s) "USERNAME") "Username for authentication",
	Option ['p'] ["password"] (ReqArg (\s -> Password s) "PASSWORD") "Password for authentication"
	]

modifyConnection :: ConnectInfo -> PgArguments -> ConnectInfo
modifyConnection connection (Ip ip) = connection {connectHost = ip, connectPort = 5432} 
modifyConnection connection (DbName name) = connection {connectDatabase = name}
modifyConnection connection  (Username user) = connection {connectUser = user}
modifyConnection connection (Password password) = connection {connectPassword = password}

-- TODO change this hideous function
evalArgs :: IO ( Maybe ConnectInfo )
evalArgs = do
	args <- getArgs
	progName <- getProgName
	let a = getOpt RequireOrder options args
	case a of
		([], _, _) -> (error $ printUsage progName) >> return Nothing
		(flags, [], []) -> return . Just $ createConnection flags
	 	(_, nonOpts, []) -> (error $ unrecognizedArgs nonOpts) >> return Nothing
	 	(_, _, msgs) -> (error $ concat msgs ++ printUsage progName) >> return Nothing
	 where
	 	printUsage p = (usageInfo (header p) options)
	 	createConnection xs = foldl (modifyConnection) defaultConnectInfo xs
	 	unrecognizedArgs nonOpts = "unrecognized arguments: " ++ unwords nonOpts



runScripts :: PgConnection ()
runScripts = do
	let runAndPrintQuery q = q >>= pgPrintResult

	pgPutDoc "TOP TABLES BY SIZE" biggestTableText
	runAndPrintQuery ((pgRunQuery biggestTableSelect) :: PgConnection [BiggestTableResult]) 

	pgPutDoc "TOP ACCESSED TABLES" mostAccessedTableText
	runAndPrintQuery ((pgRunQuery mostAccessedTableSelect) :: PgConnection [MostAccessedTableResult]) 

	pgPutDoc "TABLES WITH WORST BUFFER HIT AVERAGE" sharredBufferTableText
	runAndPrintQuery ((pgRunQuery sharredBufferTableSelect) :: PgConnection [SharredBufferTableResult]) 
	
	pgPutDoc "BIGGEST TABLES WHERE INDEX SCANS ARE LESS THAN 80%" leastUsedIndexesText   
	runAndPrintQuery ((pgRunQuery leastUsedIndexesSelect) :: PgConnection [LeastUsedIndexesResult]) 

	pgPutDoc "TOP WRITE-INTENSIVE TABLES" tableSizeAndWritesText   
	runAndPrintQuery ((pgRunQuery tableSizeAndWritesSelect) :: PgConnection [TableSizeAndWritesSelectResult]) 

	pgPutDoc "AVG. BUFFER HIT RATIO OVER DATABASE" sharredBufferOverallText 
	runAndPrintQuery ((pgRunQuery sharredBufferOverallSelect) :: PgConnection [SharredBufferOverallResult]) 
		
	pgPutDoc "LEAST ACCESSED INDEXES IN DATABASE" unusedIndexesText 
	runAndPrintQuery ((pgRunQuery unusedIndexesSelect) :: PgConnection [UnusedIndexesResult]) 

	pgPutDoc "POTENTIALLY STUCK TRANSACTIONS" transactionStuckText 
	runAndPrintQuery ((pgRunQuery transactionStuckSelect) :: PgConnection [TransactionStuckResult]) 
	
	pgPutDoc "LONGEST RUNNING TRANSACTION IN SECONDS" findLongestTransactionText 
	runAndPrintQuery ((pgRunQuery findLongestTransactionSelect) :: PgConnection [FindLongestTransactionResult]) 

	pgPutDoc "LONGEST RUNNING QUERIES" findLongestQueryText 
	runAndPrintQuery ((pgRunQuery findLongestQuerySelect) :: PgConnection [FindLongestQueryResult]) 

	pgPutDoc "UNVACUUMED OR UNANALYZED TABLES (OLDER THAN 14 DAYS)" lastAnalyzedAndVacuumedText
	runAndPrintQuery ((pgRunQuery lastAnalyzedAndVacuumedSelect) :: PgConnection [LastAnalyzedAndVacuumedResult]) 


main = do
        c <- evalArgs
        case c of
        	(Just connection) -> runReaderT runScripts connection
        	Nothing -> return ()
	
