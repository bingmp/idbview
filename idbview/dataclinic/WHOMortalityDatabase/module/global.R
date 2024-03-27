
theme_select <- c('bw','classic','classic','linedraw','cleveland','dark','grey','gray','get',
                  'light','replace','minimal','pubclean','void','test','update','transparent')

basic_plot_UI <- function(id) { # basic ggplot ui setting
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width =2,
             dropdownButton(circle=FALSE, label = lang$t("basic"),size = "sm", br(),br(),
                            fluidRow(
                              column(width = 12, selectInput(ns('theme'),label = "theme",selected = 'void',choices = theme_select )),
                              column(width = 12, textInput(ns('title'), label = "plot.title", value = "WHO Mortality Database" )),
                              # column(width = 6, textInput(ns('x.axis.title'), label = "x.title", value = "" )),
                              # column(width = 6, textInput(ns('y.axis.title'), label = "y.title", value = "" ) ),
                              # column(width = 6, textInput(ns("color_name"),label = "color.name",value = "") ),
                              # column(width = 6, selectInput(ns("color_type"), label = 'color.theme', color_type ) )
                              column(width = 12, textInput(ns("fill_name"),label = "fill.name",value = "value") )
                              # column(width = 6, selectInput(ns("fill_type"), label = 'fill.theme', color_type) )
                            )
             ) ),
      column(width =2,
             dropdownButton(circle=FALSE, label= lang$t("title"), size = "sm",br(),br(),
                            fluidRow(
                              column(width = 12,numericInput(ns('size_title'),  'size', value = 25 )),
                              column(width = 12,selectInput(ns('color_title'),  "color",
                                                            choices = colors(), selected = "black" )),
                              column(width = 12,numericInput(ns('hjust_title'), "hjust",value = 0.5 )),
                              column(width = 12,numericInput(ns('vjust_title'), 'vjust', value = 0 )),
                              column(width = 12,numericInput(ns('angle_title'), 'angle', value = 0 ))
                            )
             ) ) ) 
  ) 
}

download_plot_UI <- function(id){# download plot ui setting
  ns <- NS(id)
  tagList(
    hr(),
    dropdownButton(circle=FALSE, label=lang$t("download plot"), status="success",icon = icon("download"),
                   br(),br() ,
                   numericInput(inputId  = ns('w'),    label = lang$t("plot.weight"),value = 12),
                   numericInput(inputId  = ns('h'),    label = lang$t("plot.high"),value = 8),
                   numericInput(inputId  = ns('ppi'),  label = lang$t("plot.dpi"),value = 150),
                   downloadBttn(outputId = ns("pdf") , label = "PDF" , size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("png") , label = "PNG" , size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("jpeg"), label = "JPEG", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("tiff"), label = "TIFF", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("rds"),  label = "RDS",  size='sm', block=TRUE )
    )
  )
}
