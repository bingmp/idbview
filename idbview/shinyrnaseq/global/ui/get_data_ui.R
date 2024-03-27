
get_data_UI <- function(id) { # data get panel ui setting
  ns <- NS(id)
  # tagList(
  # bs4Dash::tabsetPanel(
  tabPanel(title = 'Data',
           fluidRow(
             box(title=lang$t("Expression data of selected gene"),solidHeader=TRUE,
                 width=9,status='primary',background = "white",
                 splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("table") ) ) ),
             box(width = 3,status="success",
                 actionBttn( inputId = ns("show"), label = lang$t("Show Data"),
                             style = "fill", color = "primary", size = "sm" ),
                 hr(),
                 selectInput(ns('group_select'),  lang$t("group: control vs case") , choices = c("") ,  multiple = T ),
                 selectInput(ns('var_select'),  lang$t("var column") , choices = c("") ,  multiple = T )
             )
           )
  )
}
