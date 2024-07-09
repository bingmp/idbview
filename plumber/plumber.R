# 1、 cp ~/code/plumber.R ~/ShinyApps/plumber/
# 2、 cp ~/shinyApps/plumber/plumber.R ~/plumber/plumber.R
library("plumber")
library("RMySQL")
library("stringr")
library('magrittr')
library('dplyr')


# 0、连接 mysql
con <- dbConnect(MySQL(), user = 'shiny', password = 'pbm242813', dbname = 'idbview',
                 host = 'mysql')



# 1、RNAseq 数据集 --------------------------------------------------------------
# 1、RNAseq 数据集
RNAseq_dataset <- dbGetQuery(con, "SELECT * FROM all_dataset")
RNAseq_dataset$url <- paste0(
  "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",
  stringr::str_split_fixed(RNAseq_dataset$gse, "GPL", 2)[, 1]
)

# 1.1 全部
#' GEO all data sets
#' @get /api/geo/data/datasets
function(req) {
  data <- RNAseq_dataset
  return(data)
}

# 2.2 数据集计数
geo_data_count <- table(RNAseq_dataset $disease) %>% 
  data.frame() %>% 
  dplyr::rename_with(~ c('name', 'value') )

#' GEO data data count
#' @get /api/geo/data/count
function(req) {
  data <- geo_data_count
  return(data)
}

# 2、RNAseq 分组信息 --------------------------------------------------------------------

# 2.1、RNAseq 数据集分组信息
RNAseq_group <- dbGetQuery(con, "select * FROM all_group")
RNAseq_group$url <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", RNAseq_group$gsm)

# 分组信息
#' GEO data group sets
#' @get /api/geo/group/groups
function(req) {
  data <- RNAseq_group
  return(data)
}


# 2.2 样本计数
geo_group_count <- table(RNAseq_group$disease) %>% 
  data.frame() %>% 
  dplyr::rename_with(~ c('name', 'value') )

#' GEO data group count
#' @get /api/geo/group/count
function(req) {
  data <- geo_group_count
  return(data)
}

# 3 Clinic ------------------------------------------------------------------

# 3.1 临床数据：CHCMU 分组信息
chcmu_group <- dbGetQuery(con, "SELECT * FROM clinic_chcmu_group")

#' CHCMU data group
#' @get /api/clinic/chcmu/groups
function(req) {
  data <- chcmu_group
  return(data)
}

# 3.2、临床数据：世界卫生组织疾病死亡率数据
who_group <- dbGetQuery(con, "SELECT * FROM clinic_who_group")

# WHO 死亡率数据信息
#' WHO data count
#' @get /api/clinic/who/groups
function(req) {
  data <- data.frame(name=who_group$Disease)
  data$data <- who_group[,3:ncol(who_group)-1]
  return(data)
}


# 3.3 临床数据：nCov2019 每天的实时数据
nCov2019 <- dbGetQuery(con, 'select * FROM Clinic_nCov2019' )

#' Clinic nCov2019 data group
#' @get /api/clinic/ncov19/byday
function(site,day,req) {
  data <- subset(nCov2019,time==day & site==site)
  return(data)
}

# 3.4 临床数据：疾病与样本计数
clinic_count <- dbGetQuery(con, 'select * FROM clinic_all_group' )

#' Clinic all (CHCMU + WHO + nCvoid19) datasets
#' @get /api/clinic/all/datasets
function(req) {
  data <- clinic_count[,c('name','dataset')] %>% dplyr::rename_with(~ c('name', 'value') )
  return(data)
}

#' Clinic all (CHCMU + WHO + nCvoid19) data samples
#' @get /api/clinic/all/groups
function(req) {
  data <- clinic_count[,c('name','sample')] %>% dplyr::rename_with(~ c('name', 'value') )
  return(data)
}



# 4 默认配置 --------------------------------------------------------------------

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "*")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  plumber::forward()
}