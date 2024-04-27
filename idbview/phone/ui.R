library(shiny)
name <- c(
  "两江职能系列", "两江临床医技系列", "两江门诊系列", "两江感染系列",
  "渝中职能系列", "渝中临床医技系列", "渝中门诊系列"
)
# Define UI for application that draws a histogram
fluidPage(
  selectInput("bumen", label = "", choices = name, selected = name[1], multiple = F),
  DT::dataTableOutput("table")
)
