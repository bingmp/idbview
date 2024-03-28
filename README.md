# idbview

#### Introduction

**idbview** is a respiratory disease database that integrates Rshiny-based data analysis and visualization tools.

#### Architecture

Front-end: HTML, CSS, JavaScript, and vue3

Back-end: R

Mysql: mysql5.7

# Install

#### Docker install rstudio-server and shiny-server

-   Docker install

``` shell
docker run --name rshiny -d \
-e USER=bingm \
-e PASSWORD=123456 \
-e ROOT=TRUE \
-e PERUSER=FALSE \
--network test-network --network-alias rshiny \
-p 3839:3838 -p 8788:8787 \
-v /home/pengbm/home/shiny:/home/bingm/ShinyApps \
ccr.ccs.tencentyun.com/bingmp/docker-rshiny
```

-   Docker install mysql

``` shell
# add user: shiny
# add 3 file for mysql: 
/home/shiny/mysql/data
/home/shiny/mysql/conf
/home/shiny/mysql/log

# install mysql and mount file.
docker run -d --name mysql \
--network test-network --network-alias mysql \
--cpus=2  \
-p 3308:3306 \
-v /home/shiny/mysql/data:/var/lib/mysql \
-v /home/shiny/mysql/conf:/etc/mysql \
-v /home/shiny/mysql/log:/var/log/mysql \
-e TZ=Asia/Shanghai \
-e MYSQL_ROOT_PASSWORD=123456 \
mysql:5.7
```

-   vue code

``` shell
git clone git@gitee.com:bingmp/r-vue.git
pnpm run install
pnpm run dev
```

#### Instructions

1.  idbview: <https://idbview.com/>

#### Contribution

1.  All the above code is built by Bingmp.
