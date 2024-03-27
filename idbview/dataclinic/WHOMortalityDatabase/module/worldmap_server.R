
# library('DT')
library('sf')
library('ggspatial')
library('ggplot2')
# library('plotly')
# library('ggplotify')
# showtext::showtext_auto() # library("showtext") # Chinese text 

worldmapServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Load the data
      dataAll <- reactive( readRDS("./www/dataAll.RDS") )
      
      region <- reactive(readRDS('./www/region.RDS'))

      dataDisease <- reactive( subset(dataAll(), Disease == input$dataDisease ) ) # Select disease
      
      observe({ # Select year
        Year <- sort(unique(dataDisease()$Year),decreasing = T )
        updateSelectizeInput(session,server = T,'dataYear', choices = Year )
        
        myData <- reactive({
          if( input$dataAge == "" | input$dataSex == "" | input$dataYear == ""){ return(NULL)}
          if( length(intersect(input$dataYear, Year))<1 ){ return(NULL)}
          myData <- subset(dataDisease(), Year == input$dataYear & Sex == input$dataSex & AgeGroup == input$dataAge
          )[,c('CountryName','Year','Sex','AgeGroup',input$dataIndex)]
          return(myData)
        })
        output$myData <- DT::renderDataTable( myData() ) # Show select data in UI.
        
        # output header summary 
        output$summary_disease <- renderValueBox({
          validate(need(input$dataDisease != "" & input$dataYear != "", "Loading"))
          x = length( Disease )
          valueBox(
            paste0(x, " Disease"), myData()$Year[1],
            icon = icon("hospital"), color = "warning")
        })

        output$summary_country <- renderValueBox({
          validate(need(input$dataDisease != "" & input$dataYear != "", "Loading"))
          x = nrow(myData())
          valueBox(
            paste0(x, " Country"), 
            myData()$Year[1], icon = icon("earth"), color = "maroon")
        })
        
        labels <- reactive({ # merge select data and map data by region( country )
          if( is.null(myData() ) ){return(NULL)}
          myData <- myData()[,c('CountryName',input$dataIndex)]
          colnames(myData) <- c('region','value')
          labels <- merge(region(), myData , by = 'region')
          
          return( labels )
        })
        
        plot <- reactive({ # plot selected result data with world map.
          if( is.null(labels() ) ){ return(NULL)}
          
          labels <- labels()
          world <- map_data("world")
          world[which(world$region=='Taiwan'),'region'] <- "China Taiwan"
          world[which(world$subregion == 'Hong Kong'),'region'] <- 'China HK'
          world$value <- 0
          for (i in 1:nrow(labels)) { # merge data with value
            world[which(world$region==labels$region[i] ),'value'] <- labels[which(labels$region==labels$region[i]),'value']
          }
          
          p <- ggplot() +
            geom_polygon(data = world, 
                         aes( x = long, y = lat,group=group, fill=value),show.legend = T,
                         color= input$line ) +
            scale_fill_gradient( low= input$low, high= input$high )
          # +
          #   geom_text(data = labels ,
          #             aes(x = long, y= lat, label =label),
          #             colour = input$label_color,
          #             size = input$size
          #             )
          
          # 添加比例尺
          if(input$annotation_scale=="show"){
            p <- p + annotation_scale(location='bl',plot_unit = "km") 
          }
          
          # 添加指北针
          if(input$annotation_north_arrow=="show"){
            p <- p + annotation_north_arrow(location = "tl", which_north = "false",
                                            style = north_arrow_fancy_orienteering)
          }
          
          p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
          
          if(!input$title==''){ p <- p + ggtitle( input$title ) }

          if(!input$fill_name==''){ # 填充图例名
            fill <- paste0("labs(fill='",input$fill_name,"')")
            p <- p + eval(parse(text = fill ))
          }
          
          p <- p +
            theme( plot.title   = element_text(size  = input$size_title,
                                              hjust = input$hjust_title,
                                              color = input$color_title,
                                              vjust = input$vjust_title,
                                              angle = input$angle_title ),
                   legend.position = input$legend.position
                   )
          return(p)
        })
        
        output$plot0 <- renderPlot({
          validate(need(input$dataYear != "", "Loading"))
          plot()
        })
        
        if(T){ # 2、下载图片与数据
          output$pdf <- downloadHandler(filename="map.pdf",
                                        content = function(file){ pdf(file,width=input$w,height=input$h); print(plot() ); dev.off() }
          )
          output$png <- downloadHandler(filename="map.png",
                                        content = function(file){png(file,width=input$w,height=input$h,units="in",res=input$ppi);print(plot());dev.off() }
          )
          output$jpeg <- downloadHandler(filename="map.jpeg",
                                         content = function(file){jpeg(file,width=input$w,height=input$h,units="in",res=input$ppi); print(plot());dev.off() }
          )
          output$tiff <- downloadHandler(filename="map.tiff",
                                         content = function(file){tiff(file,width=input$w,height=input$h,units="in",res=input$ppi); print(plot()); dev.off()} 
          )
          output$rds <- downloadHandler(filename="map.RDS",
                                        content = function(file){ saveRDS(plot(), file) } )
          output$downloadData <- downloadHandler(filename = function() {paste('Data.csv') } ,
                                                 content = function(file) {write.csv(myData() , file, row.names = F, fileEncoding = "GB18030") }  
          )
        }
      })
    }
  )
}
