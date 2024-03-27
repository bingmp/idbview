
source('module/ui_global.R')
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")

heatmapUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  # tagList(
  bs4Dash::tabsetPanel(
    get_data_UI("heatmap"), 
    tabPanel(title = 'Heatmap',
             fluidRow(
               box(title="pheatmap: heatmap of gene expression",width=9,solidHeader=TRUE,status = "primary",background = "white",
                   splitLayout(cellWidths = c("100%"),plotOutput(ns("plot"),height = 500 ) ) ),
               box(width=3,status="success",
                   seting_UI("heatmap"),
                   selectInput( inputId = ns("plot_group"),  label = lang$t("group or var"),
                                c('group','var') ,selected = "group" ),
                   selectInput(ns('color1'),lang$t("Low-value color"),colors(),selected = "navy"),
                   selectInput(ns('color2'),lang$t("Middle-value color"),colors(),selected = "white"),
                   selectInput(ns('color3'),lang$t("High-value color"),colors(),selected = "firebrick3"),
                   dropdownButton(circle=FALSE, label= "Plot Seting",icon = icon("image"),br(),br(),
                                  fluidRow(
                                    column(width = 6,textInput(ns('group_name'),label = "group.name",value = 'group') ),
                                    column(width = 6,textInput(ns('var_name'),label = "var.name",value = 'var')),
                                    column(width = 6,selectInput(ns('rowname'),"row.name",choices = c("show"='T',"hide"='F'),selected = 'T') ),
                                    column(width = 6,selectInput(ns("scale"),"scale", selected = 'row',
                                                                 choices = c("row"='row',"column"='column','none'='none') ) ),
                                    column(width = 6,selectInput(ns('colname'),"col.name",choices = c("show"='T',"hide"='F'),selected = 'F') ),
                                    column(width = 6,selectInput(ns("angle_col"),"angle.colname",choices = c('270', '0', '45', '90', '315'),selected = '45' ) ),
                                    column(width = 6,selectInput(ns('row_cluster'),"row.cluster",choices = c("Yes"='T',"No"='F'),selected = 'T') ),
                                    column(width = 6,selectInput(ns('col_cluster'),"row.cluster",choices = c("Yes"='T',"No"='F'),selected = 'F') ),
                                    column(width = 6,selectInput(ns('data_log'),"log2",choices = c(T,F),selected = F) ),
                                    column(width = 6,selectInput(ns('disp_num'),"display.num",choices = c("show"='T',"hide"='F'),selected = 'F') )
                                  ) ),
                   download_plot_UI("heatmap")  )
             )
    )
  )
  # ) # tagList
} # function(id)
