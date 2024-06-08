# idbview

#### Introduction

**idbview** is a respiratory disease database that integrates Rshiny-based data analysis and visualization tools.  

#### Architecture

Front-end: HTML, CSS, JavaScript, and vue3  

Back-end: R  

Mysql: mysql5.7  

# Install

#### Docker install rstudio-server and shiny-server

- 1 Docker rshiny install

``` shell
docker pull pengbm/rshiny

docker run --name rshiny -d \
-e USER=bingm \
-e PASSWORD=yourPassword \
-e ROOT=TRUE \
-e PERUSER=FALSE \
--network test-network --network-alias rshiny \
-p 3838:3838 -p 8787:8787 \
-v /home/pengbm/home/shiny:/home/bingm/ShinyApps \
pengbm/rshiny:v1
```
1.2 details:  
shiny-server: ip:3838; (or https://db.chcmu.com.cn)  

rstudio-server: ip:8787;  user: bingm;  password: yourPassword


- 2 Docker mysql install  
2.1 install  
``` shell
# install mysql.

docker pull mysql:5.7

docker run -d --name mysql \
--network test-network --network-alias mysql \
--cpus=2  \
-p 3306:3306 \
-e TZ=Asia/Shanghai \
-e MYSQL_ROOT_PASSWORD=yourPassword \
mysql:5.7
```

2.2 details  
```r
# file: "~/ShinyApps/global/server/conMysql.R"
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
2.3 our demo mysql connect  
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
