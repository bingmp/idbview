
source('module/ui_global.R')

lang <- Translator$new(translation_csvs_path = "./lang/info/")

logisticsUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  bs4Dash::tabsetPanel(
    get_data_UI("logistics") ,
    tabPanel(title = "Logistic plot",
             fluidRow(
               box(title="Logistic regression plot",solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   basic_plot_UI("logistics")  ),
               box(width=3,status="success",
                   seting_UI("logistics"),
                   selectInput( inputId = ns("plot_group"),  label = lang$t("choose plot"),selected = "ROC.train",
                                c("ROC.train","ROC.test")  ),
                   sampling_UI("logistics"),
                   roc_plot_UI("logistics"),
                   download_plot_UI("logistics"),br(),
                   dropdownButton(circle=FALSE, label = lang$t("nomogram plot"), status="success",icon = icon("download"),
                                  br(),br() ,
                                  downloadBttn(outputId = ns("nomogram_rms.train") , label = "nomogram_rms.train", size='sm', block=TRUE )
                   )
                   
               )
             ) ),
    tabPanel(title = "Logistic table",
             fluidRow(
               box(title="Logistic regression result table",solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   machine_table_UI("logistics")  
               ),
               box(width=3,status="success",
                   download_table_UI("logistics") )
             )
    )
  ) }
