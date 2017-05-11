#ssh -i ~/.ssh/id_rsa -L 33306:web.ghtorrent.org:3306 ghtorrent@web.ghtorrent.org
#ps axuf | grep ssh
#mysql -u ght -h 127.0.0.1 -P 33306 ghtorrent

library(tidyverse)
#library("reshape2")
library("RMySQL")

#require(devtools)

#rm(list = ls(all = TRUE))

mysql.user   =  "ght"
mysql.passwd = ""
mysql.db     = "ghtorrent"
mysql.host   = "127.0.0.1"
mysql.port = 33306
paralll      = 4

ghtor = dbConnect(dbDriver("MySQL"),
                  user = mysql.user,
                  password = mysql.passwd,
                  dbname = mysql.db,
                  host = mysql.host,
                  port = mysql.port)

#dbClearResult(dbListResults(ghtor)[[1]])
#dbListFields(ghtor,"projects")
#dbListTables(ghtor)


my_db = src_mysql("ghtorrent", host = "127.0.0.1", port = 33306, user = "ght", password = "")
#my_db %>% tbl(sql("SELECT * FROM followers limit 5"))
#my_db %>% tbl("followers") %>% filter(user_id == '5') %>% select(follower_id, user_id)
