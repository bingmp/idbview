
# library('readxl')
# library('htmlwidgets')
library('rhandsontable')
library('ggplot2')
# apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev
library('sf') 
library('ggspatial')
# library('showtext') # 解决画图中文乱码
showtext::showtext_auto()

mapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$plot0 <- renderPlot({ return(NULL) })
      output$plot1 <- plotly::renderPlotly({ return(NULL) })
      
      sample <- reactive({
        data <- readRDS(('www/map_labels.RDS'))
        
        if(input$labels=="fullname"){
          data$labels <- data$name
        }
        else if(input$labels=="english"){
          data$labels <- data$english
        }
        else if(input$labels=="city"){
          data$labels <- data$city
        }
        else if(input$labels=="province"){
          data$labels <- data$province
        }
        else(
          data$labels <-""
        )
        
        data <- data[,c("name","labels","value")]
        
        return(data)
      })
      
      observeEvent(input$show, {
        
        # Load the data # 读取数据
        df1 <- reactive({
          file1 <- input$file
          if ( is.null(file1) ){
            data <- sample()
          } 
          else{
            d <- tail( unlist(strsplit(file1$datapath,'[.]') ), 1)
            if( d=='csv' ){
              data <- data.frame( read.csv(file1$datapath,header=T, stringsAsFactors = FALSE, fileEncoding = 'GB18030') )
            } else{
              data <- data.frame( readxl::read_excel(file1$datapath,1) )
            } 
          } # else
          return( data )
        })
        
        output$table <- renderRHandsontable(
          if( is.null(df1() ) ){return(NULL)}else{
            return( rhandsontable(df1(),rowHeaderWidth = 22,height = 500) %>% 
                      hot_cols(columnSorting = TRUE) %>% 
                      hot_col('name', readOnly = TRUE) %>% 
                      hot_col('value',type = 'numeric'))
          }
          
        )
        
        observeEvent(input$submit, {
          
          # 整合数据 china_data
          data_map <- reactive({
            
            if( is.null(df1() ) ){return(NULL)}
            
            df1 <- as.data.frame( hot_to_r( input$table ) )
            # df1$adcode <- as.character(df1$adcode)
            
            df0 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")[,c('adcode','name','center','geometry')]
            
            df0$labels <- ''
            df0$value <- 0
            
            for (i in 1:nrow(df1) ) {
              df0[which(df0$name ==df1$name[i] ),c('labels',"value")] <- df1[i,c('labels','value')]
            }
            
            df0$jd <- c(matrix(unlist(df0$center[1:34]),ncol = 2, byrow = T)[,1], 116.36832 )
            df0$wd <- c(matrix(unlist(df0$center[1:34]),ncol = 2, byrow = T)[,2], 39.91508)
            
            if(input$size==0){ df0$labels <- '' }
            
            df0$value <- as.numeric(df0$value )
            
            return(df0)
            
          })
          
          # 编写函数
          myfun <- reactive({
            
            if( is.null(df1() ) ){return(NULL)}
            
            data_map <- data_map() 
            
            p <- ggplot(data=data_map)+ geom_sf(aes(fill = value) ) + 
              fixed_plot_aspect(ratio = 1.25)
            # coord_sf(crs = 4326)
            
            # 添加比例尺
            if(input$annotation_scale=="show"){
              p <- p + annotation_scale(location='bl',plot_unit = "km") 
            }
            # 添加指北针
            if(input$annotation_north_arrow=="show"){
              p <- p + annotation_north_arrow(location = "tl", which_north = "false",
                                              style = north_arrow_fancy_orienteering)
            }
            
            p <- p +
              # 颜色
              scale_fill_gradient( low= input$low, high= input$high )+
              # 标签
              geom_text(data = data_map,
                        aes(x=jd, y=wd, label= labels),
                        position = "identity",
                        colour = input$label_color,
                        size = input$size)
            
            p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
            
            if(!input$title==''){
              p <- p + ggtitle( input$title )
            }
            if(!input$x.axis.title==''){
              p <- p + xlab( input$x.axis.title )
            }
            if(!input$y.axis.title==''){
              p <- p + ylab( input$y.axis.title )
            }
            
            # labs(color=input$color_name) # 图例名
            # labs(fill=input$fill_name)
            # if(!input$color_name==''){
            #   color <- paste0("labs(color='",input$color_name,"')")
            #   p <- p + eval(parse(text = color ))
            # }
            if(!input$fill_name==''){ # 填充图例名
              fill <- paste0("labs(fill='",input$fill_name,"')")
              p <- p + eval(parse(text = fill ))
            }
            
            p <- p +
              theme(
                # 标题设置
                plot.title   = element_text(size  = input$size_title,
                                            hjust = input$hjust_title,
                                            color = input$color_title,
                                            vjust = input$vjust_title,
                                            angle = input$angle_title
                ),
                # 坐标轴标题
                axis.title   = element_text(size  = input$size_axis.title,
                                            color = input$color_axis.title,
                                            hjust = input$hjust_axis.title,
                                            vjust = input$vjust_axis.title,
                                            angle = input$angle_axis.title
                ),
                # 坐标轴标签
                axis.text    = element_text(size  = input$size_axis.text,
                                            color = input$color_axis.text,
                                            hjust = input$hjust_axis.text,
                                            vjust = input$vjust_axis.text,
                                            angle = input$angle_axis.text
                ),
                # 坐标轴刻度
                axis.ticks   = element_line(linewidth = input$size_axis.ticks,
                                            linetype  = input$linetype_axis.ticks,
                                            color     = input$color_axis.ticks
                ),
                legend.position = input$legend.position,  # # c( "left","top", "right" , "bottom")
                # 图例标签
                legend.title = element_text(size  = input$size_legend.title,
                                            hjust = input$hjust_legend.title,
                                            color = input$color_legend.title,
                                            vjust = input$vjust_legend.title,
                                            angle = input$angle_legend.title
                ), 
                # 图例文字
                legend.text  = element_text(size  = input$size_legend.text,
                                            hjust = input$hjust_legend.text,
                                            color = input$color_legend.text,
                                            vjust = input$vjust_legend.text,
                                            angle = input$angle_legend.text
                )
              )
            
            # 配色
            # p <- p + eval(parse(text = paste0("scale_color_",input$color_type,"()")))
            # p <- p + eval(parse(text = paste0("scale_fill_",input$fill_type,"()")))
            
            return( p)
            
          } )
          
          output$plot0 <- renderPlot({
            if( is.null(df1() ) ){return(NULL)}
            return( myfun() )
            
          }  )
          
          
          if(T){
            output$pdf <- downloadHandler(
              filename="map.pdf",
              content = function(file){
                pdf(file,width=input$w,height=input$h)
                print(myfun() )
                dev.off()
              }
            )
            output$png <- downloadHandler(
              filename="map.png",
              content = function(file){
                png(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              }
            )
            output$jpeg <- downloadHandler(
              filename="map.jpeg",
              content = function(file){
                jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              } )
            
            output$tiff <- downloadHandler( 
              filename="map.tiff",
              content = function(file){
                tiff(file,width=input$w,height=input$h,units="in",res=input$ppi)
                print(myfun() )
                dev.off()
              } )
            
            output$rds <- downloadHandler( 
              filename="map.RDS",
              content = function(file){
                saveRDS(myfun(),file = file)
              } )
          }

        })
      } )  #  observeEvent(input$submit, {
      
      # 下载参考数据
      output$downloadSampleData <- downloadHandler(
        filename = function() {
          paste('ChinaMap_data.csv')
        },
        content = function(file) {
          write.csv(sample(), file,  row.names = F, fileEncoding = 'GB18030')
        } )
      
    } # function(input, output, session)
  ) # moduleServer
} # function(id) 
