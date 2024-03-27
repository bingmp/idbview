
library('rhandsontable')
library('shinycssloaders') # 加载界面
source('../global/ui/basic_plot_UI.R')
source('../global/ui/downloadPlot.R')
# # File with translations
lang <- Translator$new(translation_csvs_path = "./lang/info/")
shiny.i18n::usei18n(lang)
lm_dataUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Input Data"),width=9,solidHeader=TRUE,status='primary',background = "white" ,
          splitLayout(cellWidths = c("60%"), rHandsontableOutput(ns("table") ) ) ),
      box(width = 3,status="success",
          fileInput(ns("file1"), label = lang$t("Input file"),multiple = FALSE ),
          h6(lang$t('Format: .csv .xlsx .xls')),
          actionBttn( inputId = ns("show"), label = "Show Data", 
                      style = "fill", color = "primary", size = "sm" ),
          hr(),
          column(width = 12,selectInput(ns('factor'),  h6(lang$t("Categorical variables"),style="color:orange"), c(""), multiple = T )),
          column(width = 12,selectInput(ns('factor_order'),  h6(lang$t("Ordered variables"),style="color:orange"), c(""), multiple = T )),
          column(width = 12,selectInput(ns('numeric'), h6(lang$t("Continuous variables"),style="color:orange"), c(""), multiple = T )),
          hr(),
          downloadButton(ns("downloadtable"),lang$t("SampleData") )  ) )
  )
}

lm_plotUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title=lang$t("Fitting Plot"),width=9,solidHeader=TRUE,status='primary',background = "white",
          basic_plot_UI('lm')
          ),
      box(width = 3,status="success",
          actionBttn( inputId = ns("submit"), label = "Analyze Data", 
                      style = "fill", color = "primary", size = "sm" ),hr(),
          fluidRow(
            column(width = 6, selectInput(ns("x"), label = 'x', c("") ) ),
            column(width = 6, selectInput(ns("y"), label = 'y', c("") ) ),
            column(width = 6, selectInput(ns("color"), label = 'color', c("") ) ),
            column(width = 6, selectInput(ns("fill"),  label = 'fill' , c("") ) )
          ),
          br(),
          dropdownButton( label = lang$t("setting"),icon = icon('image'),circle = FALSE,width = NULL,br(),br(),
                          fluidRow(
                            column( width = 6, selectInput(ns("method"), label = lang$t('Fitting mode'),
                                                           selected = 'lm', 
                                                           choices = c('loess','lm','glm','gam') ) ),
                            column( width = 6, selectInput(ns("plot"), label = lang$t('Graphic selection'),
                                                           selected = "sn",
                                                           choices = c("scatter"="s",
                                                                       "fitting"="n", 
                                                                       "merge"="sn") ) ),
                            column( width = 6, textInput(ns("formula"), label = lang$t('Fitting equation'),value = "y~x" ) ),
                            column( width = 6, selectInput(ns("se"),  label = lang$t('Confidence interval'),
                                                           choices = c('show'= 'T',
                                                                       'hide'= 'F'),
                                                           selected = 'T' ) ),
                            column( width = 6,numericInput(ns("se_level"),label = lang$t("CI level"), value = 0.95)),
                            column( width = 6, selectInput(ns("cor"), label = lang$t('Correlation'),
                                                           choices = c("pearson"="pearson", 
                                                                       "kendall"="kendall",
                                                                       "spearman"="spearman",
                                                                       "none"= F ),
                                                           selected = "pearson" ) ),
                            column( width = 6,numericInput(ns("size_plot"),label = lang$t("point.size"), value = 1) ),
                            column( width = 6,numericInput(ns("size_line"),label = lang$t("line.size"), value = 1) ),
                            column( width = 6, selectInput(ns('face'),label = lang$t('facet or not'),
                                                           choices = c('Yes'=T, "No"=F),
                                                           selected = T) )
                          )
          ),br(),
          download_plot_UI('lm')    )
    ) # fluidRow
  )
}

lm_helpUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(width=12,title="使用说明",solidHeader=TRUE,status='primary',background = "white",height="100%",
                 tags$h2("使用说明"),
                 tags$hr(),
                 tags$h6(lang$t("1、参考数据中，x、y列分别为相关性的两变量，group为分组（color/fill），face为分面（可无）。
                             group对应分组，face对应分面，列名不可改变，但对应列内容可自行编辑。")),
                 tags$h6(lang$t("2、R 与 P仅为直线相关性，对应拟合为 lm, y~x ，其他拟合方式的时候，恐不具有代表性。")),
                 tags$h6(lang$t("3、主题、点线、填充颜色提供数种默认选择。")),
                 tags$h6(lang$t("4、方程中的x与y的对应关系有:")),
                 tags$h6(lang$t("y ~ x ")),
                 tags$h6(lang$t("y ~ poly(x, n)")),
                 tags$h6(lang$t("y ~ log(x)")),
                 tags$h6(lang$t("等。其中 y ~ poly(x, n) 为多项式关系，n为项数，为1、2、3等整数，
                             如 y ~ poly(x, 2) 表示二项式。"))
                 
    )
    ) )
  
}

lmUI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::tabsetPanel(
      tabPanel(title = 'Data', lm_dataUI("lm") ),
      tabPanel(title = 'Plot', lm_plotUI("lm") ),
      tabPanel(title = lang$t("Help"), lm_helpUI("lm") )
    )
  ) # NS(id)
} # function(id)
