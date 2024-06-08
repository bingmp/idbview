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
docker pull pengbm/rshiny

docker run --name rshiny -d \
-e USER=bingm \
-e PASSWORD=123456 \
-e ROOT=TRUE \
-e PERUSER=FALSE \
--network test-network --network-alias rshiny \
-p 3838:3838 -p 8787:8787 \
-v /home/pengbm/home/shiny:/home/bingm/ShinyApps \
pengbm/rshiny:v1
```
shiny-server: ip:3838; (or https://hiplotdev.hiplot.com.cn)  
rstudio-server: ip:8787; uer: bingm; password: 123456

-   Docker install mysql

``` shell
# add user: shiny
# add 3 file for mysql: 
# /home/shiny/mysql/data
# /home/shiny/mysql/conf
# /home/shiny/mysql/log

# install mysql and mount file.
docker run -d --name mysql \
--network test-network --network-alias mysql \
--cpus=2  \
-p 3306:3306 \
# -v /home/shiny/mysql/data:/var/lib/mysql \
# -v /home/shiny/mysql/conf:/etc/mysql \
# -v /home/shiny/mysql/log:/var/log/mysql \
-e TZ=Asia/Shanghai \
-e MYSQL_ROOT_PASSWORD=123456 \
mysql:5.7
```

-   vue code

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
