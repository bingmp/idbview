
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Atelectasis Data"),
  br(),
  dataTableOutput('table')
)
