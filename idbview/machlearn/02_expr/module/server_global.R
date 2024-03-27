
library("magrittr")
# library("DT")
# library("showtext") #中文问题
showtext::showtext_auto()

source(paste0(server,'conMysql.R') )
# get data
source(paste0(server,"get_data_server.R") )
# ggplot
source(paste0(server,"ggplot_server.R") )
# download plot
source(paste0(server,"downloadPlot_server.R") )
