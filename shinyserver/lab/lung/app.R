
# 加载包
library(shiny)
library(shinydashboard)
library(rhandsontable)
# library(readxl)

myfun <- function(df0){
  
  df1 <- df0 ; df1 <- as.data.frame(df1)
  
  colnames(df1) <- c('parameter', 'Penh' )
  
  # 取各组小鼠名称与计数
  a <- names( which( table(df1$parameter) >= 2  ) ) 
  num <- table(df1$parameter) 
  
  # 将组名加到数据列
  for (i in 1:length(a)) { 
    df1[which(df1== a[i]),2]<-a[i]
  }
  
  # 去除缺失行
  df1 <- df1[which(!is.na(df1$Penh)),]
  
  
  v0 <- df1[,2] # 取数据列赋值到新建向量v0
  
  # 循环参数准备
  l <- length(a)
  df2 <- data.frame(matrix(NA,ncol = l))
  colnames(df2) <- c(a)
  
  # 提取数据循环
  for(i in 1:length(a)) {
    
    n1 <- num[a[i]]
    
    v1 <- v0[(which(v0 == a[i])[1]+1):(which(v0 == a[i])[n1]-1)]
    df2[1:length(v1),i] <- v1
    
    # v2 <- v0[(which(v0 == a[i])[1]):(which(v0 == a[i])[n1] )]
    # df2[1:length(v2),i+l+1] <- v2
    
  }
  
  for(i in 1:dim(df2)[2]) {
    
    df2[which(is.na(df2[,i])),i] <- ''
    
  }
  
  return(df2)
  
}

# 设置shiny界面参数
header <- dashboardHeader(title = "") # 页面标题

# 可输入、输出的按钮 
sidebar <- dashboardSidebar(
  fileInput("file", "Format: excel( csv xls xlsx)", multiple = TRUE),
  
  actionButton( inputId = "submit", label = "Analyze Data" ),
  tags$hr(),
  # Horizontal line ----
  column(width = 12,downloadButton("downloadSampleData", "Demo/示例数据") ),
  br(), br(),
  column(width = 12,downloadButton("downloadData", "Result/处理结果") )
)

# 网页呈现的内容 
body <- dashboardBody( 
  tabsetPanel(
    tabPanel(  title = 'Format/参考格式',
               fluidRow(
                 column(width = 12, height =0,
                        box(width = 12, height =0,status = "primary", solidHeader = T ) , 
                        rHandsontableOutput("sample") 
               )
               )),
    tabPanel(title = 'Result/处理结果',
      fluidRow( column(width =12,
                       box(width = 12, height = 0, status = "primary", solidHeader = T ) , 
                      dataTableOutput("results") ) 
      ) )
  )

)

# 设置 ui
ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  
  observeEvent(input$submit, { 
    
    df1 <- reactive({
      
      file1 <- input$file
      if(is.null(file1)){
        data <- read.csv('./www/df.csv')
        return(data) 
      }
      
      d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
      if( d=='csv' ){
        data <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
      } else{
        library(readxl)
        data <- data.frame( readxl::read_excel(file1$datapath,1) )
      } 
      return(data)
      
    })
    
    if(!is.null(df1() ) ){
      output$sample <- renderRHandsontable(
        rhandsontable(df1(),rowHeaderWidth = 22, width = 240, height = 500) %>% 
          hot_cols(columnSorting = TRUE)
      )
    }
    
    # 3、分析情况展示
    output$results <- renderDataTable({
      
      if (is.null(input$sample ) ) { return() }
      df1 <- hot_to_r(input$sample  )
      
      df2 <- myfun(df0 = df1() )
      return(df2)
    })
    
    
    # 5、依据函数计算结果，并设置可下载
    output$downloadData <- downloadHandler(
      
      filename = function() {
        paste("result.csv")
      },
      content = function(file) {
        df2 <- myfun(df0 = df1() )
        write.csv(df2, file,row.names = F, fileEncoding = 'GB18030')
      } )
    
    }  )

  
  # 下载参考数据
  output$downloadSampleData <- downloadHandler(
    filename = function() {
      paste('sample.csv')
    },
    content = function(file) {
      data <- read.csv('./www/df.csv')
      write.csv(as.data.frame(data), file, row.names = F, fileEncoding = 'GB18030')
    } ) 
  
}

shinyApp(ui, server)
