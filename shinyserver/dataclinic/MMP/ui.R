
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Mycoplasma Genetic Resistance Mutation Data"),
  br(),
  dataTableOutput('table')
)
