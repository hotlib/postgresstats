# postgresstats
A haskell program for dumping useful statistics about a database in PostgresSQL. Basic usage:

```sh
runhaskell stats.hs --ip=192.168.10.160 --db=mydb --user=postgres --password=postgres
```
Note you should provide user credentails which have admin permissions in the given databse.

The result will be a dump which checks the following:
+   top tables by size,
+   tables that are most access in the database,
+   tables with worst buffer hit average,
+   tables where index scans are less than 80%,
+   tables where most writes occur,
+   least accessed indexes in the database,
+   transactions that are running suspiciously long,
+   longest running transactions,
+   longest running queries,
+   tables that have been not vacuumed or analysed for longer than 14 days.
   
### TODO
Tested only on PostgreSQL 9.2 
