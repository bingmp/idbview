
library("magrittr")
# library("DT")
# showtext::showtext_auto() # 中文问题

# mysql
source(paste0(server,'conMysql.R') )
# # get data
source(paste0(server,"get_data_server.R") )
# # ggplot
# source(paste0(path,"ggplot_server.R") )
# # download plot
# source(paste0(path,"downloadPlot_server.R") )