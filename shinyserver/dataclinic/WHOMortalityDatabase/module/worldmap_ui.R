
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")
source('module/global.R')

Disease <- c("Asthma" = "Asthma",
             "Chronic obstructive pulmonary disease" = "ChronicObstructivePulmonaryDisease",
             "Covid19" = "Covid19",
             "Lower birth weight" ="LowerBirthWeight",
             "Lower respiratory infections" ="LowerRespiratoryInfections",
             "Other respiratory diseases" ="OtherRespiratoryDiseases", 
             "Protein energy malnutrition" ="ProteinEnergyMalnutrition",
             "Tuberculosis" ="Tuberculosis",
             "Upper respiratory infections" ="UpperRespiratoryInfections")

AgeGroup <- c('All','0','1-4', '5-9', '10-14','15-19',
              '20-24','25-29','30-34','35-39',
              '40-44','45-49' ,'50-54','55-59',
              '60-64','65-69','70-74','75-79',
              '80-84','85+')

Index <- c('Death rate per 100000 population'='Death_rate_per_100000_population',
           'Percentage of cause specific deaths out of total deaths'='Percentage_of_cause_specific_deaths_out_of_total_deaths',
           'Number'='Number')

worldmapUI <- function(id) {
  ns <- NS(id)
  shiny.i18n::usei18n(lang)
  tagList(
    fluidRow(
      box(title = tags$a(href="https://platform.who.int/mortality",
                         "WHO Mortality Database (https://platform.who.int/mortality)", target="_blank"),
          width=9, status = "info", solidHeader= T,# background = "white",
          fluidRow( # header summary
            valueBoxOutput(width = 6,outputId=ns("summary_disease")),
            valueBoxOutput(width = 6,outputId=ns("summary_country")) 
          ),
          bs4Dash::tabsetPanel(
            tabPanel(title = 'Plot',br(),
                     basic_plot_UI('worldmap'),br(),
                     splitLayout(cellWidths = c("100%"),plotOutput(ns("plot0"), height = 600 ) ) ),
            tabPanel(title = 'Table',br(),
                     downloadButton(ns("downloadData") ,'Data Table'),br(),br(),
                     DT::dataTableOutput(ns('myData') )
            )
          )
      ),
      box(width=3,status="success",
          fluidRow(
            # tags$a(href="https://platform.who.int/mortality",
            #        "WHO Mortality Database", target="_blank"),
            column(width = 12,selectInput(inputId = ns('dataDisease'),label = 'Disease',choices = Disease,selected = Disease[1] )),
            
            column(width = 12,selectInput(inputId = ns('dataIndex'),label = 'Select',
                                          choices = Index,
                                          selected = 'Death_rate_per_100000_population') ),
            column(width = 5,selectInput(inputId = ns('dataYear'),label = 'Year', choices = '',selected = '')),
            column(width = 4,selectInput(inputId = ns('dataAge'),label = 'Age',choices = AgeGroup,selected = AgeGroup[1]) ),
            column(width = 3,selectInput(inputId = ns('dataSex'),label = 'Sex',choices = c('All','F' = 'Female','M' = 'Male'),selected = "All" )),
          ),hr(),
          fluidRow(
            column(width = 6,selectInput(inputId = ns('annotation_scale'),label = lang$t('scale'),
                                         choices = c("show","hide"), selected = "show") ),
            column(width = 6,selectInput(inputId = ns('annotation_north_arrow'),label = lang$t('north.arrow'),
                                         choices = c("show","hide") ,selected = "show" ) ),
            column(width = 6,selectInput(inputId = ns("low") , lang$t("Low-value color"), colors() , selected = "white") ),
            column(width = 6,selectInput(inputId = ns("high"), lang$t("High-value color"), colors() , selected = "red" ) ),
            # column(width = 6,numericInput(inputId = ns('size'),label = lang$t('Label font size'),value = 4     ) ),
            # column(width = 6,selectInput(inputId = ns('label_color'),label = lang$t('label.color'),choices = colors(),selected = "black") ),
            column(width = 6,selectInput(inputId = ns("line"), lang$t("line.color"), colors() , selected = "gray60"   ) ),
            column(width = 6,selectInput(ns('legend.position'),lang$t('legend.position'),selected = 'right',
                                         choices = c( "left","top", "right" , "bottom")))
          ),
          download_plot_UI('worldmap')
      )
    )
  )
}
