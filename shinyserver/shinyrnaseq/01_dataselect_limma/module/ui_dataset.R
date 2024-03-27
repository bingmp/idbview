
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

datasetUI <- function(id) {
  ns <- NS(id)
  bs4Dash::tabsetPanel(
    tabPanel(title = 'Dataset',
             box(title=lang$t("Dataset of pediatric respiratory disease"),solidHeader=TRUE,
                 width=12,status='primary',background = "white",
                 splitLayout(cellWidths = c("100%"),
                             DT::dataTableOutput(ns("dataset") ) )
             )
    ),
    tabPanel(title = 'Data select',
             fluidRow(
               box(title=lang$t("Dataset of pediatric respiratory disease"),solidHeader=TRUE,
                   width=9,status='primary',background = "white",
                   # splitLayout(cellWidths = c("100%"),
                   #             DT::dataTableOutput(ns("dataset") ) ),
                   # hr(),
                   splitLayout(cellWidths = c("100%"),
                               DT::dataTableOutput(ns("gse_group") ) )
               ),
               box(width = 3,status="success",
                   selectInput(ns('choose_gse'), "Choose data", choices = c(""), multiple = F ),
                   hr(),
                   shinyWidgets::actionBttn( inputId = ns("show_gse"), label = "Submit Select",
                               style = "fill", color = "primary", size = "sm" ),
                   hr(),
                   fluidRow(
                     column(width = 12,selectInput(ns('choose_condition'),  "condition" , 
                                                   choices = c("") ,  multiple = T ) ),
                     column(width = 12,selectInput(ns('choose_cluster'),  "cluster" , 
                                                   choices = c("") ,  multiple = T ) ),
                     column(width = 6,selectInput(ns('choose_sample'),  "sample" , 
                                                  choices = c("") ,  multiple = T ) ),
                     column(width = 6,selectInput(ns('choose_gender'),  "gender" , 
                                                  choices = c("") ,  multiple = T ) ),
                     column(width = 6,numericInput(ns('choose_age_min'), "min age", value = 0) ),
                     column(width = 6,numericInput(ns('choose_age_max'), "max age", value = 100 ) )
                     
                   )
               )
             )
    ),
    tabPanel(title = 'Gene Expression',
             fluidRow(
               box(title=lang$t("Expression data of selected gene"),width=9,solidHeader=TRUE,status='primary',background = "white",
                   splitLayout(cellWidths = c("100%"),
                               DT::dataTableOutput(ns("gse_exp") ) )
               ),
               box(width = 3,status="success",
                   shinyWidgets::actionBttn( inputId = ns("show_learning_data"), label = "Show Data",
                               style = "fill", color = "primary", size = "sm" ),
                   hr(),
                   fluidRow(
                     column(width = 12,selectInput(ns('choose_learning_column'), "choose var column → group" , 
                                                   choices = c("gsm","condition","cluster","sample",
                                                               "gender" ), 
                                                   selected = 'condition',  multiple = F ) ),
                     
                     column(width = 6,selectInput(ns('choose_con'),  "group: con" , 
                                                  choices = c( ) ,  multiple = T ) ),
                     column(width = 6,selectInput(ns('choose_case'),  "group: case" , 
                                                  choices = c( ) ,  multiple = T ) ),
                     column(width = 6,textInput(ns('con_name'),  "con rename" ,value = "" ) ),
                     column(width = 6,textInput(ns('case_name'),  "case rename" , value = "") ),                     
                     # column(width = 12,selectizeInput(ns('choose_gene'),  "choose gene" , 
                     #                                  choices = c("") ,  multiple = T ) )
                     column(width = 12,selectInput(ns('exp_group'),  "exp or group" , 
                                                   choices = c("exp","group"),
                                                   selected = "group", 
                                                   multiple = F ) )
                   ),
                   hr(),
                   fluidRow(
                     column(width = 12,
                            shinyWidgets::actionBttn( inputId = ns("submit_data"), label = "Submit Data",
                                        style = "fill", color = "primary", size = "sm" )  ),
                     br(),br(),
                     column(width = 12,
                            shinyWidgets::actionBttn( inputId = ns("remove_data"), label = "Remove Data",
                                        style = "fill", color = "primary", size = "sm" ) )
                   )
               )
             )
    )
  ) }
