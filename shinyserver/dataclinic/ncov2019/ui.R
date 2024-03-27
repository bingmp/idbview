
library('shiny')
# library('DT')
library('shinydashboard')
library('plotly')

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19"),
  
  dashboardSidebar(collapsed = T,
    # tags$br(),
    # actionButton("showData", "Show data"),
    # tags$hr(),
    # choose country
    selectizeInput(
      'country', 'Choose Country', choices = NULL,
      options = list(
        placeholder = 'Choose Country',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),

    # choose province 
    conditionalPanel('["China", "South Korea", "United States", "Japan", "Iran", "Italy", "Germany", "United Kingdom"].indexOf(input.country) > -1', 
                     selectizeInput('province', 'Choose Province', 
                                    choices = c("Select Province" =  NULL  ),
                                    options = list( placeholder = 'Province')),
                     tags$p("")),
    
    # choose city
    conditionalPanel('["China"].indexOf(input.country) > -1', 
                     selectizeInput('city', 'Choose City', 
                                    choices = NULL,
                                    options = list( placeholder = 'City')),
                     tags$p("")),
    # choose date
    selectizeInput('date_t', 'Newest date', choices = NULL)

  ),
  
  dashboardBody(
    # header summary
    fluidRow(
      valueBoxOutput(outputId="summary_confirm"),
      valueBoxOutput(outputId="summary_cure"),
      valueBoxOutput(outputId="summary_dead")
    ),  

    fluidRow(
      # data table
      box(title = downloadButton('dataDownload', 'Data Table'	),
          solidHeader = T,
          width = 4,
          collapsible = T,
          DT::dataTableOutput("data_table"),
          style = "font-size: 70%;"),
      
      # line plot
      box(title = "Plot", solidHeader = T,
          width = 8, collapsible = T,
          plotly::plotlyOutput("line_plot") )
    ),  

    fluidRow(
      tabBox(
        width=12,
        title = "",
        selected = "Global Confirmed",
        tabPanel("Global Confirmed", plotOutput("worldwide_plot")),
        tabPanel("Global Mortality Rate", plotly::plotlyOutput("Mortality_plot")),
        tabPanel("Global Health Rate", plotly::plotlyOutput("Health_plot")),
        tabPanel("Country Statistics", plotOutput("country_plot")),
        tabPanel("Growth Rate", plotly::plotlyOutput("growth_rate")),
        tabPanel("Forecast", 
                 fluidPage(
                   # setting days to forecast
                   sliderInput("num", "days to forecast",
                               min = 1, max = 10,
                               value = 5),
                   plotOutput("forecast") )
                 ) 
      )
    ),
    tags$a(href='https://github.com/GuangchuangYu/nCov2019',target="_blank",
           "Revise form nCov2019: Github", style="font-size: 15px")
    # end row
    
  ) # end dashboard body 
  
) # end UI
