
source('module/ui_global.R')
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

svmUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  # tagList( 
  bs4Dash::tabsetPanel(
    get_data_UI("svm") ,
    tabPanel(title = "SVM table",
             fluidRow(
               box(title=lang$t("Support Vector Machines"),solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   machine_table_UI("svm")
               ),
               box(width=3,status="success",
                   seting_UI("svm"),
                   sampling_UI("svm"),
                   fluidRow(
                     column(width = 6,numericInput(ns("cost_svm"),label = 'cost', value = NA,min = 0)),
                     column(width = 6,numericInput(ns("gamma_svm"),label = 'gamma', value = NA,min = 0))
                   ),
                   span(textOutput(ns("gamma_cost")), style="color:orange"),hr(),
                   download_table_UI("svm")
               )
               
             )
    ),
    tabPanel(title = 'SVM plot',
             fluidRow(
               box(title=lang$t("Support Vector Machines"),solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   basic_plot_UI("svm")  ),
               box(width=3,status="success",
                   actionBttn( inputId = ns("submit_svm"), label = lang$t("Analyze Data"),
                               style = "fill", color = "primary", size = "sm" ),hr(),
                   download_plot_UI("svm"),hr(),
                   fluidRow(
                     column(width = 12, span(textOutput(ns("gamma_recmmond")), style="color:orange") ),
                     column(width = 12, span(textOutput(ns("cost_recmmond")), style="color:orange") ),
                     column(width = 6,numericInput(ns("cost_min"),label = lang$t('cost min'),
                                                   value = 0.1) ),
                     column(width = 6,numericInput(ns("gamma_min"),label = lang$t('gamma min'),
                                                   value = 0.1) ),
                     column(width = 6,numericInput(ns("cost_max"),label = lang$t('cost max'),
                                                   value = 1) ),
                     column(width = 6,numericInput(ns("gamma_max"),label = lang$t('gamma max'),
                                                   value = 1) ),
                     column(width = 6,numericInput(ns("cost_interval"),label = lang$t('cost interval'),
                                                   value = 0.2) ),
                     column(width = 6,numericInput(ns("gamma_interval"),label = lang$t('gamma interval'),
                                                   value = 0.2) )
                   )
               ) 
             ) )
  )
  # )
}
