
source('module/ui_global.R')
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

dtreeUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  # tagList( 
  bs4Dash::tabsetPanel(
    get_data_UI("dtree") ,
    tabPanel(title = 'Decision tree plot',
             fluidRow(
               box(title=lang$t("Condition Inference Tree"),solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   basic_plot_UI("dtree")  ),
               box(width=3,status="success",
                   seting_UI("dtree"),
                   selectInput( inputId = ns("plot_group"),  label = lang$t("choose plot"),
                                c("Dtree","ROC.train","ROC.test") ,selected = "Dtree" ),
                   sampling_UI("dtree"),
                   roc_plot_UI("dtree"),
                   download_plot_UI("dtree")
               ) 
             ) ),
    tabPanel(title = "Decision tree table",
             fluidRow(
               box(title=lang$t("Condition Inference Tree"),solidHeader=TRUE,
                   width=9,status = "primary",background = "white",
                   machine_table_UI("dtree") 
               ),
               box(width=3,status="success",
                   download_table_UI("dtree") )
             )
    )
  )
  # )
}
