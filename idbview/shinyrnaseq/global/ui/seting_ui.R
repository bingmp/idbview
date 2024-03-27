
seting_UI <- function(id){ # data select panel ui setting
  ns <- NS(id)
  tagList(
    actionBttn( inputId = ns("submit"), label = lang$t("Analyze Data"),
                style = "fill", color = "primary", size = "sm" ),hr(),
    selectInput( inputId = ns("age_and_genes"),  label = lang$t("age and genes"),  c( ),multiple = T )
  )
}