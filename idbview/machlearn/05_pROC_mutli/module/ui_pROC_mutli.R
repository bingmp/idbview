
source('module/ui_global.R')
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

roc_multiUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  bs4Dash::tabsetPanel(
    get_data_UI("roc_multi"),
    tabPanel(title = 'ROC plot',
             fluidRow(
               box(title="pROC: multi-gene ROC plot",width=9,solidHeader=TRUE,status = "primary",background = "white",
                   basic_plot_UI("roc_multi")  ),
               box(width=3,status="success",
                   seting_UI("roc_multi"),
                   # selectInput( inputId = ns("plot_group"),  label = lang$t("contrast: group or var"),
                   #              c('group') ,selected = "group" ),
                   selectInput(ns("direction"), label = lang$t("direction"), c("auto", "<", ">"),selected = "auto" ),
                   dropdownButton(circle=FALSE, label= "plot setting", size = "sm",br(),br(),
                                  fluidRow(
                                    column(width = 6, selectInput(ns("best_point"), label = 'best.point', c("show","hide"),selected = "show" ) ),
                                    column(width = 6, numericInput(ns("point_size"),label = "point.size",value = 4) ),
                                    column(width = 6, selectInput(ns("auc"), label = 'AUC', c("show","hide"),selected = "show" ) ),
                                    column(width = 6, numericInput(ns("auc_size"),label = "auc.size",value = 4) ),
                                    column(width = 6, numericInput(ns("linetype"),label = "line.type",value = 2) ),
                                    column(width = 6, numericInput(ns("linesize"),label = "line.size",value = 1) ),
                                    column(width = 6, selectInput(ns("show_legend"), label = 'legend', c("show","hide"),selected = "show") ),
                                    column(width = 6, selectInput(ns("facet"), label = 'face', c("row","column","none"),selected = "row") )
                                  )
                   ),
                   download_plot_UI("roc_multi") , br(),
                   dropdownButton(circle=FALSE, label = lang$t("download table"), status="success",icon = icon("download"),
                                  br(),br() ,
                                  downloadBttn(outputId = ns("table1"),  label = "data.auc", size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("table2"),  label = "best.point", size='sm', block=TRUE )
                   )
               )
             ) )
  )
}
