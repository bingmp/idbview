
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Mycoplasma Genetic Resistance Mutation Data"),
  br(),
  dataTableOutput('table')
  # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #     sidebarPanel(
  #     ),
  # 
  #     # Show a plot of the generated distribution
  #     mainPanel(
  #       dataTableOutput('table')
  #     )
  # )
)
