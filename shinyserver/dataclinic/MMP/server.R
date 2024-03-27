# library('RMySQL')
# mysql
path <- "~/ShinyApps/global/server/"
source(paste0(path, "conMysql.R"))

server <- function(input, output) {
  output$table <- renderDataTable({
    con <- eval(parse(text = mysql_con))
    mydata <- dbGetQuery(con, "SELECT * FROM Clinic_MycoplasmaGeneticResistanceMutation")
    dbDisconnect(con)
    return(mydata)
  })
  for (i in dbListConnections(MySQL())) {
    dbDisconnect(i)
  }
}
