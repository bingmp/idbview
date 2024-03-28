
source('module/ui_global.R')
lang <- Translator$new(translation_csvs_path = "./lang/info/")

procUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  bs4Dash::tabsetPanel(
    get_data_UI("proc"),
    tabPanel(title = 'ROC plot',
             fluidRow(
               box(title="pROC: single gene ROC plot",width=9,solidHeader=TRUE,status = "primary",background = "white",
                   basic_plot_UI("proc") ),
               box(width=3,status="success",
                   seting_UI("proc"),
                   selectInput( inputId = ns("plot_group"),  label = lang$t("choose"),  c( ) ),
                   selectInput(ns("direction"), label = lang$t("direction"), c("auto", "<", ">"),selected = "auto" ),
                   roc_plot_UI("proc"),
                   download_plot_UI("proc") ,  br(),
                   dropdownButton(circle=FALSE, label=lang$t("download table"), status="success",icon = icon("download"),
                                  br(),br() ,
                                  downloadBttn(outputId = ns("table0"),  label = "data.auc", size='sm', block=TRUE ),
                                  downloadBttn(outputId = ns("table1"),  label = "best.point", size='sm', block=TRUE )
                   )
               )
             ) )
  )
}

