
source('module/server_global.R')
procServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        df <- get_df("select_data")
        if(is.null(df() ) ){
          output$table <- DT::renderDataTable( {
            df <- data.frame(`中文`="请在 dataset 面版选择、提交数据后再进行此步分析。",
                             English="Please choose and submit data in dataset panel before this analysis."
            ) %>% t()
            colnames(df) <- "The submit data cannot be found."
            return(df)
          } )
        }
        else{
          output$table <- DT::renderDataTable( df() ) 
          
          observe( { 
            df <- df() #  %>% dplyr::select(c("group",input$age_and_genes))
            updateSelectInput(session,"group_select" , choices = unique(df$group) ,
                              selected = unique(df$group)  )
            updateSelectInput(session,"var_select" , choices = unique(df$var) ,
                              selected = unique(df$var))
            updateSelectInput(session,"age_and_genes" , choices = colnames(df)[3:ncol(df)] ,
                              selected = colnames(df)[3:ncol(df)] )
            
            observe( { 
              # if(!is.null( input$age_and_genes ) ){
              #   df <- df() %>%  dplyr::select(c("group","var",input$age_and_genes)) 
              # }else{
              #   df <- df()
              # }
              df <- df() %>%
                subset(eval(parse(text = paste('group'," =='", input$group_select ,"'",sep = '', collapse = '|'))) ) %>%
                subset(eval(parse(text = paste('var'," =='", input$var_select ,"'",sep = '', collapse = '|'))) )
              df$group <- factor(df$group,levels = input$group_select)
              # df$var <- factor(df$var,levels = input$var_select)
              
              updateSelectInput(session,"plot_group" , choices = input$age_and_genes ,
                                selected = input$age_and_genes[1] )
              
              observeEvent(input$submit, {
                roc_list <- reactive({
                  if(is.null(input$age_and_genes)){return(NULL)}
                  roc_list <- roc(eval(parse(text = paste("group ~ ",input$plot_group,sep = '') )) ,
                                  direction= input$direction,
                                  ci=T,
                                  aur=T,
                                  percent=F,
                                  data=df[,-2])
                  return(roc_list)
                })
                data.auc <- reactive({
                  if(is.null( roc_list() )){return(NULL)}
                  data.auc <- roc_list() %>% ci() %>% data.frame() %>% t()  %>%
                    as.data.frame() %>% round( digits = 3)
                  colnames(data.auc) <- c("ci_lower","auc","ci_upper")
                  data.auc$name <- input$plot_group
                  return(data.auc)
                })
                best.point <- reactive({
                  if(is.null( roc_list() )){return(NULL)}
                  roc.list <- roc_list()
                  best.point <- coords(roc.list, "best", ret=c("threshold", "sensitivity","1-specificity", "npv","ppv"))
                  best.point <- round(best.point[1,],3)
                  best.point <- data.frame(best.point)
                  colnames(best.point) <- c("threshold", "sensitivity","1-specificity", "npv","ppv")
                  best.point$name <- input$plot_group
                  return(best.point)
                })
                plot <- reactive({
                  if(is.null( roc_list() )){return(NULL)}
                  roc_list <-roc_list()
                  data.auc <- data.auc()
                  best.point <- best.point()
                  
                  p <- ggroc(roc_list, legacy.axes = T,
                             linetype = input$linetype,
                             color    = input$linecolor, 
                             size     = input$linesize) +
                    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                                 linetype = input$linetype, 
                                 color    = input$linecolor,
                                 size     = input$linesize, 
                                 show.legend= F)
                  
                  if(input$auc=="show"){
                    p <- p+
                      geom_text(data.auc, hjust = 0.5,
                                show.legend= F,
                                size= input$auc_size, 
                                color   = input$textcolor,
                                mapping= aes(0.6, 0.4, 
                                             label =paste(input$plot_group," AUC=",auc,"\n","95% CI: ",ci_lower,"~",ci_upper)) )
                  }
                  
                  if(input$best_point=="show"){
                    p <- p +
                      geom_point(data = best.point,show.legend = F,size= input$point_size/2, 
                                 mapping = aes(x = `1-specificity`,y = sensitivity),
                                 color   = input$textcolor)+
                      geom_text(data = best.point, 
                                show.legend = F,
                                size= input$point_size, 
                                color   = input$textcolor,
                                mapping = aes(x = `1-specificity`+0.1,y = sensitivity,
                                              label=paste(threshold,"\n","(",sensitivity,",",`1-specificity`,")")) )
                    
                  }
                  
                  p <- ggplot_plot(p,input)+
                    theme_axis.title(input) +
                    theme_axis.text(input) +
                    theme_axis.ticks(input) +
                    theme_legend.title(input) +
                    theme_legend.text(input)
                  
                  return(p)
                  
                })
                
                output$plot <- renderPlot({return( plot() ) })
                
                # download plot: pdf, png, jpeg, tiff, rds
                output$pdf0  <- eval(parse(text = output_plot[1] ))
                output$png0  <- eval(parse(text = output_plot[2] ))
                output$jpeg0 <- eval(parse(text = output_plot[3] ))
                output$tiff0 <- eval(parse(text = output_plot[4] ))
                output$rds0  <- eval(parse(text = output_plot[5] ))
                
                # download table
                if(T){
                  output$table1 <- downloadHandler(filename="data_auc.csv",
                                                   content = function(file){write.csv(data.auc() , file, row.names = F ) }  )
                  output$table2 <- downloadHandler(filename="best_point.csv",
                                                   content = function(file){write.csv(best.point() , file, row.names = F)  }  )
                }
                
              } )        
              
            })
          })
        }
        
      })
      
    } ) }
for(i in dbListConnections(MySQL()) ){dbDisconnect(i)}