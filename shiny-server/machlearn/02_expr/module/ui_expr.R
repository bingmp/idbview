
source('module/ui_global.R')
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

exprUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  # tagList(
    bs4Dash::tabsetPanel(
      get_data_UI("expr"), 
      tabPanel(title = 'Boxplot',
               fluidRow(
                 box(title= "Boxplot of gene expression",width=9,solidHeader=TRUE,status='primary',background = "white",
                     basic_plot_UI("expr") 
                 ),
                 box(width = 3,status="success",
                     seting_UI("expr"),
                     selectInput( inputId = ns("plot_group"),  label = lang$t("group or var"),
                                  c('group','var') ,selected = "group" ),
                     dropdownButton(circle=FALSE, label= "diff.test",
                                    br(),br() ,
                                    selectInput( inputId = ns("method"),  label = "test method",selected = "none",
                                                 c('none',"anova",'t.test','wilcox.test','kruskal.test') ),
                                    numericInput(inputId = ns('label.x'),label = "p value x.position", value = 0),
                                    numericInput(inputId = ns('label.y'),label = "p value y.position", value = 0)  
                                    
                     ),
                     download_plot_UI("expr")
                 ) ) # fluidRow    
      ) 
    )
  # )  # tagList
} # function(id)
