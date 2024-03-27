
roc_plot_UI <- function(id) {# roc plot ui
  ns <- NS(id)
  dropdownButton(circle=FALSE, label= lang$t("ROC plot setting"), size = "sm",br(),br(),
                 fluidRow(
                   column(width = 6, selectInput(ns("best_point"), label = 'best.point', c("show","hide"),selected = "show" ) ),
                   column(width = 6, numericInput(ns("point_size"),label = "point.size",value = 4) ),
                   column(width = 6, selectInput(ns("auc"), label = 'AUC', c("show","hide"),selected = "show" ) ),
                   column(width = 6, numericInput(ns("auc_size"),label = "auc.size",value = 4) ),
                   column(width = 6, numericInput(ns("linetype"),label = "line.type",value = 2) ),
                   column(width = 6, numericInput(ns("linesize"),label = "line.size",value = 1) ),
                   column(width = 6, selectInput(ns("linecolor"), label = 'line.color', colors(),selected = "rosybrown" ) ),
                   column(width = 6, selectInput(ns("textcolor"), label = 'text.color', colors(),selected = "pink3" ) )
                 )
  )
}