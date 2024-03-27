
source('module/server_global.R')
roc_multiServer <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        
        df <- get_df("select_data")
        if(is.null(df() ) ){
          output$table <- not_get_df
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
                              selected = colnames(df)[-(1:3)] )
            
            observe( { 
              #   df <- df %>%  dplyr::select(c("group","var",input$age_and_genes))
              df <- df() %>% 
                subset(eval(parse(text = paste('group'," =='", input$group_select ,"'",sep = '', collapse = '|'))) ) %>%
                subset(eval(parse(text = paste('var'," =='", input$var_select ,"'",sep = '', collapse = '|'))) )
              df$group <- factor(df$group,levels = input$group_select)
              # df$var <- factor(df$var,levels = input$var_select)
              
              observeEvent(input$submit, {
                roc_list <- reactive({
                  if(is.null( input$age_and_genes )|length(input$age_and_genes)<2){return(NULL)}
                  roc_list <- roc(eval(parse(text = paste("group ~ ", paste(input$age_and_genes, collapse =" + ")) )) ,
                                  direction= input$direction,
                                  ci=T,
                                  aur=T,
                                  percent=F,
                                  data=df)
                  return(roc_list)
                })
                data.auc <- reactive({
                  if(is.null( roc_list() )){return(NULL)}
                  roc.list <- roc_list()
                  data.auc <- do.call(rbind, lapply(roc.list, ci) )
                  data.auc <- round(data.auc,3)
                  data.auc <- data.frame(data.auc)
                  colnames(data.auc) <- c("ci_lower","auc","ci_upper")
                  data.auc$name <- rownames(data.auc)
                  return(data.auc)
                })
                best.point <- reactive({
                  if(is.null( roc_list() )){return(NULL)}
                  best.point <- roc_list() %>% 
                    lapply(function(x){
                      coords(x, "best", ret=c("threshold", "sensitivity","1-specificity", "npv","ppv"))[1,]}) %>% 
                    do.call(what = rbind ) %>%  round(digits = 3) %>% data.frame()
                  colnames(best.point) <- c("threshold", "sensitivity","1-specificity", "npv","ppv")
                  best.point$name <- rownames(best.point)
                  return(best.point)
                  
                })
                
                plot <- reactive({
                  if(is.null( roc_list() )){return(NULL)}
                  roc_list <-roc_list()
                  data.auc <- data.auc()
                  best.point <- best.point()
                  p <- ggroc(roc_list, legacy.axes = T,
                             aes=c("color") , 
                             linetype= input$linetype, 
                             linewidth= input$linesize,
                             show.legend= (input$show_legend=='show') ) +
                    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                                 linetype= input$linetype, 
                                 size= input$linesize, 
                                 show.legend= F)
                  
                  if(input$auc=="show"){
                    p <- p+
                      geom_text(data.auc, hjust = 0.5,show.legend= F,size= input$auc_size, 
                                mapping= aes(0.6, 0.4, 
                                             label =paste("AUC=",auc,"\n","95% CI: ",ci_lower,"~",ci_upper)) )
                  }
                  
                  if(input$best_point=="show"){
                    p <- p +
                      geom_point(data = best.point,show.legend = F,size= input$point_size/2, 
                                 mapping = aes(x = `1-specificity`,y = sensitivity) )+
                      geom_text(data = best.point, show.legend = F,size= input$point_size, 
                                mapping = aes(x = `1-specificity`+0.1,y = sensitivity,
                                              label=paste(threshold,"\n","(",sensitivity,",",`1-specificity`,")")) )
                    
                  }
                  
                  #  faceting
                  if(input$facet=="row"){
                    p <- p + facet_grid(.~name)
                  }
                  else if(input$facet=="column"){
                    p <- p + facet_grid(name~.)
                  }
                  
                  p <- ggplot_plot(p,input)+
                    theme_axis.title(input) +
                    theme_axis.text(input) +
                    theme_axis.ticks(input) +
                    theme_legend.title(input) +
                    theme_legend.text(input)
                  
                  return(p)
                  
                })
                
                output$plot <- renderPlot({return(plot() ) })
                
                # download plot: pdf, png, jpeg, tiff, rds
                output$pdf0  <- eval(parse(text = output_plot[1] ))
                output$png0  <- eval(parse(text = output_plot[2] ))
                output$jpeg0 <- eval(parse(text = output_plot[3] ))
                output$tiff0 <- eval(parse(text = output_plot[4] ))
                output$rds0  <- eval(parse(text = output_plot[5] ))
                
                # download table
                if(T){
                  output$table1 <- downloadHandler(filename="data_auc.csv",
                                                   content = function(file){write.csv(data.auc() , file, row.names = F) }  )
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