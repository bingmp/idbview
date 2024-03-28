
source('module/ui_global.R')
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

randomFUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  bs4Dash::tabsetPanel(
    get_data_UI("randomF"),
    tabPanel(title = "RF plot",
             fluidRow(
               box(title=lang$t("randomForest plot"),solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   basic_plot_UI("randomF")  ),
               box(width=3,status="success",
                   seting_UI("randomF"),
                   fluidRow(
                     column(width = 6,selectInput( inputId = ns("plot_group"),  label = lang$t("choose plot"),
                                                   c('Error',"ROC.train",'ROC.test') ,selected = "Error" ) ),
                     column(width = 6,numericInput(ns('trees'),label = lang$t('trees'),value = 500,min = 1) )
                   ),
                   sampling_UI("randomF"),
                   roc_plot_UI("randomF"),
                   download_plot_UI("randomF")
               )
             ) ),
    tabPanel(title = "RF table",
             fluidRow(
               box(title=lang$t("randomForest result data"),solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   machine_table_UI("randomF") 
               ),
               box(width=3,status="success",
                   # actionBttn( inputId = ns("submit_table"), label = lang$t("Analyze Data"),
                   #             style = "fill", color = "primary", size = "sm" ),hr(),
                   download_table_UI("randomF") ),
               box(title=lang$t("importance"),solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("df_importance") ) ,height = 600 )
               ),
               box(width=3,status="success",
                   dropdownButton(circle=FALSE, label = lang$t("download table"), status="success",icon = icon("download"),
                                  br(),br() ,downloadBttn(outputId = ns("table_importance"), label = "importance", size='sm', block=TRUE )
                   )
               )
             )
    )
  ) }
