# library("DT")
library('RMySQL')
mysql_con <- paste0("dbConnect(MySQL(),",
                    "user='idbview',",
                    "port=3306,",
                    "password='pbm242813',",
                    "dbname='idbview',",
                    "host = 'mysql')")

