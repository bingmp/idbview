
# library('DT')
# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")
source('module/ui_global.R')

# color_type <- c("npg","aaas","nejm","gsea","lancet", "rickandmorty","futurama", "tron",
#                 "startrek",  "uchicago","igv","locuszoom","d3", "ucscgb","jco","jama" )

theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

limmaUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Exp',
               fluidRow(
                 box(width=6,title=lang$t("Expression matrix"),solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("exp") ) ) ),
                 box(width=3,title=lang$t("Group information"),solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), DT::dataTableOutput(ns("group") ) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("show"), label = "Show Data",
                                 style = "fill", color = "primary", size = "sm" )
                 ) ) ),
      tabPanel(title = 'DEG',
               fluidRow(
                 box(width=9,title=lang$t("Differential genes"),solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), dataTableOutput(ns("DEG") ) ) ),
                 box(width=3,status="success",
                     actionBttn( inputId = ns("submitDEG"), label = "Analyze Data",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("untrt"), lang$t("Control group"), c("") ),
                     selectInput(ns("trt")  , lang$t("Case group"), c("") ),
                     numericInput(inputId = ns("rowSum_filter"), value = 1,
                                  label = h6(lang$t("Filter low-expression genes(rowSum ≥)")) ) ,
                     selectInput(ns("method"),label = lang$t("Adjust method"),selected = "BH",
                                 choices = c("none"="none","BH"="BH","BY" ="BY","holm"="holm") ),
                     downloadButton(ns("downloadDEG"), "DEG .csv",icon = icon('download') )
                 ) ) ),
      tabPanel(title = 'Plot',
               fluidRow(
                 box(title=lang$t("Plot"),width=9,solidHeader=TRUE,status='primary',background = "white",
                     splitLayout(cellWidths = c("100%"), plotOutput(ns("plot") ) ) ),
                 box(width = 3,status="success",
                     actionBttn( inputId = ns("submitPlot"), label = "Show Plot",
                                 style = "fill", color = "primary", size = "sm" ),hr(),
                     selectInput(ns("plot"),label = lang$t("Graphic selection"),selected = "pca",
                                 choices = c("pca"="pca",
                                             "heatmap"="heatmap",
                                             "volcano"="volcano") ),
                     
                     conditionalPanel(
                       condition = "input.plot!=='pca'",ns = NS(id),
                       dropdownButton(circle=FALSE, label=lang$t('Parameter settings'), br(),br(),
                                      conditionalPanel(
                                        condition = "input.plot=='heatmap'",ns = NS(id),
                                        fluidRow(
                                          column(width = 6,numericInput(ns("pvalue"), lang$t("pvalue"), value = 0.05) ),
                                          column(width = 6,numericInput(ns("padj")  , lang$t("padj"), value = 0.05) ),
                                          column(width = 12,numericInput(ns("logFC"),label = "logFC", value = 1) ),
                                          column(width = 6,selectInput(ns("order"),label = lang$t("order method"),selected = "pvalue",
                                                                       choices = c('pvalue'='pvalue',"logFC"="logFC") ) ),
                                          column(width = 6,numericInput(ns("heatmap_num"),label = lang$t("top genes"), value = 50) )
                                          
                                        )
                                      ),
                                      conditionalPanel(
                                        condition = "input.plot=='volcano'",ns = NS(id),
                                        fluidRow(
                                          column(width = 6,numericInput(ns("pvalue"), lang$t("pvalue"), value = 0.05) ),
                                          column(width = 6,numericInput(ns("padj")  , lang$t("padj"), value = 0.05) ),
                                          column(width = 12,numericInput(ns("logFC"),label = "logFC", value = 1) ), 
                                          column(width = 6,selectInput(ns("order"),label = lang$t("order method"),selected = "pvalue",
                                                                       choices = c('pvalue'='pvalue',"logFC"="logFC") ) ),
                                          column(width = 6,numericInput(ns("volcano_num"),label = lang$t("top labels"), value = 10) ),
                                          column(width = 6,selectInput(ns("color_down"), 'color.down', colors(),selected = 'blue' ) ),
                                          column(width = 6,selectInput(ns("color_not"), 'color.not', colors() ,selected = 'black') ),
                                          column(width = 6,selectInput(ns("color_up"), 'color.up', colors() ,'red') ),
                                          column(width = 6,selectInput(ns('theme'),lang$t('theme'),selected = 'bw',
                                                                       choices =theme_select ) )
                                          
                                        )
                                      )
                       )
                     ),
                     download_plot_UI('limma')
                 )
               ) # fluidRow 
      ),
      tabPanel(title = "Help", helpUI("expr") )
    )
  ) # tagList
} # function(id)
