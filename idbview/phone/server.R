library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  output$table <- DT::renderDataTable(
    {
      
      options(DT.options = list(pageLength = 50, searching = T, paging = T,
                                searchHighlight = TRUE,
                                language = list(
                                  info = '显示第_START_ 至 _END_ 项结果，共 _TOTAL_ 项',
                                  search = '搜索:',
                                  paginate = list(previous = '上页', `next` = '下页'),
                                  lengthMenu = '显示 _MENU_ 项结果') ) )
      subset(readRDS("phone.RDS"),部门==input$bumen)[,-1]
    },
    server = T,rownames=F
  )
}
