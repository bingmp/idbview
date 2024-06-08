# idbview

#### Introduction

**Idbview** a database and interactive platform for respiratory-associated disease.  

#### Architecture

Front-end: HTML, CSS, JavaScript, and vue3  

Back-end: R  

Mysql: mysql5.7  

# Install

#### Docker install rstudio-server and shiny-server

```sh
# create a docker network for rshiny and mysql.
docker network create docker-network
```
- 1 Docker rshiny install

``` shell
# https://hub.docker.com/r/pengbm/rshiny
docker pull pengbm/rshiny:v1

docker run --name rshiny -d \
-e USER=bingm \
-e PASSWORD=yourPassword \
-e ROOT=TRUE \
-e PERUSER=FALSE \
--network docker-network --network-alias rshiny \
-p 3838:3838 -p 8787:8787 \
pengbm/rshiny:v1
```
1.2 details:  

rstudio-server: ip:8787;  user: bingm;  password: yourPassword
your can remove "assets/" folder and "index.html" file in ~/ShinyApps/

shiny-server: ip:3838; (or https://db.chcmu.com.cn)  


- 2 Docker mysql install  
2.1 install  
``` shell
# install mysql.

docker pull mysql:5.7

docker run -d --name mysql \
--network docker-network --network-alias mysql \
-p 3306:3306 \
-e TZ=Asia/Shanghai \
-e MYSQL_ROOT_PASSWORD=yourPassword \
mysql:5.7

# create acccount "idbview" in mysql;
# create database "idbview" in mysql;
# upload data to mysql.
```
idbview.sql file at: https://db.chcmu.com.cn/idbview.sql

2.2 details  
```r
# mysql connect setting file: "~/ShinyApps/global/server/conMysql.R"
library('RMySQL')
mysql_con <- paste0("dbConnect(MySQL(),",
                    "user='idbview',",
                    "port=3306,",
                    "password='yourPassword',",
                    "dbname='idbview',",
                    "host = 'mysql')")
                    
# dbConnect(MySQL(), user = 'idbview', password = 'yourPassword', dbname = 'idbview',
# host = 'mysql')
```                 
2.3 or you can use our demo mysql connect  
```r
library('RMySQL')
mysql_con <- paste0("dbConnect(MySQL(),",
                    "user='idbview',",
                    "port=20093,",
                    "password='bingmP242813!',",
                    "dbname='idbview',",
                    "host = 'sh-cynosdbmysql-grp-lpgiq2lw.sql.tencentcdb.com')")
# dbConnect(MySQL(), user = 'idbview', password = 'bingmP242813!', dbname = 'idbview',
# host = 'sh-cynosdbmysql-grp-lpgiq2lw.sql.tencentcdb.com')
```

- 3 vue code

``` shell
git clone https://github.com/bingmp/rVue.git
cd rVue
pnpm run install
pnpm run dev
```
Front-end data are in "rVue/src/assets"  
Front-end api are in "rVue/src/api"

#### Instructions

1.  idbview: <https://idbview.com/>

#### Contribution

1.  All the above code is built by Bingmp(bingmp@stu.cqmu.edu.cn).
