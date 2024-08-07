
# library(partykit)
# library(ggparty)
source('module/server_global.R')

dtreeServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observeEvent(input$show, { 
        df <- get_df("select_data")
        if(is.null(df() ) ){
          output$table <- not_get_df
          
        }else{
          output$table <- DT::renderDataTable( df() ) 
          
          observe({
            df <- df() 
            # df[,-c(1:3)] <- apply( df[,-c(1:3)],2 ,function(x){round(x,digits = 2)})
            updateSelectInput(session,"group_select" , choices = unique(df$group) ,
                              selected = unique(df$group)  )
            updateSelectInput(session,"var_select" , choices = unique(df$var) ,
                              selected = unique(df$var))
            updateSelectInput(session,"age_and_genes" , choices = colnames(df)[3:ncol(df)] ,
                              selected = colnames(df)[4:ncol(df)] )
            
            observe({
              dfs <- reactive({
                dfs <- df() %>%  dplyr::select(c("group","var",input$age_and_genes)) %>%
                  subset(eval(parse(text = paste('group'," =='", input$group_select ,"'",sep = '', collapse = '|'))) ) %>%
                  subset(eval(parse(text = paste('var'," =='", input$var_select ,"'",sep = '', collapse = '|'))) )
                dfs$group <- factor(dfs$group,levels = input$group_select)
                # df$var <- factor(df$var,levels = input$var_select)
                return(dfs)
              })
              
              updateSliderInput(session,"size_train" , value =  as.integer(0.7*nrow(dfs() )), min = 2,max = nrow(dfs() ) )
              output$sample_size <- renderText({ paste("sample size is", nrow(dfs()) ) })
              
              observeEvent(input$submit, {
                df <- dfs()
                if( !is.na(input$set_seed) ){set.seed(input$set_seed)}
                df <- dplyr::sample_n(tbl = df , size = nrow(df ), replace = F)
                
                df.train <- reactive({
                  if(nrow(df)==0){return(NULL)}
                  if(!is.na(input$set_seed) ){set.seed(input$set_seed)}
                  df.train <- dplyr::sample_n(tbl = df, size = input$size_train, replace = F)
                  return(df.train) 
                })
                
                df.test <- reactive({  
                  if(nrow(df)==0){return(NULL)}
                  if( input$size_train < nrow(df)-1){
                    df.test <- df[setdiff( rownames(df), rownames(df.train() ) ),]
                    return(df.test) 
                  }else{ return(df) }
                })
                
                observe({
                  
                  df.train <- df.train()
                  df.test  <- df.test()
                  output$sample_size <- renderText({
                    paste("train: ",nrow(df.train),";", "test: ", nrow(df.test) )
                  })
                  
                  fit.machine <- reactive({
                    if(nrow(df.train())==0){return(NULL)}
                    if(!is.na(input$set_seed) ){set.seed(input$set_seed)}
                    fit.machine <- partykit::ctree(group~., data= df.train[,-2] )
                    return(fit.machine)
                  })
                  output$fit.machine <- downloadHandler(filename='fit.machine.RDS',content=function(file){saveRDS(fit.machine(),file)})
                  
                  df_pref.train <- reactive({# data train predict
                    if(is.null(fit.machine())){return(NULL)}
                    pred.train<- predict(fit.machine(), df.train[,-2] )
                    perf.train<- table( df.train$group,pred.train)
                    perf.train<- as.data.frame.matrix(perf.train,row.names = paste("Actual",row.names(perf.train)))
                    colnames(perf.train) <- paste("Predicted",colnames(perf.train) )
                    return(perf.train)
                  })
                  df_pref.test  <- reactive({# data test predict
                    if(is.null(fit.machine())){return(NULL)}
                    pred.test <- predict(fit.machine(), df.test[,-2] )
                    perf.test <- table( df.test$group,pred.test )
                    perf.test <- as.data.frame.matrix(perf.test,row.names = paste("Actual",row.names(perf.test)))
                    colnames(perf.test) <- paste("Predicted",colnames(perf.test ) )
                    return(perf.test)
                  })
                  df_train <- reactive({ 
                    if(is.null(fit.machine())){return(NULL)}
                    df_train <- df.train
                    df_train$pred <- predict(fit.machine(), df_train[,-2] ,type='prob')[,2] 
                    df_train$pred_group <- predict(fit.machine(), df_train[,-2] )
                    df_train <- df_train[,c(1,ncol(df_train),c(3:ncol(df_train)-1))]
                    return(df_train) 
                  })
                  df_test  <- reactive({
                    if(is.null(fit.machine())){return(NULL)}
                    df_test <- df.test
                    df_test$pred <- predict(fit.machine(), df_test[,-2] ,type='prob')[,2]
                    df_test$pred_group <- predict(fit.machine(), df_test[,-2] )
                    df_test <- df_test[,c(1,ncol(df_test),c(3:ncol(df_test)-1))]
                    return(df_test)
                  })
                  
                  plot <- reactive({
                    if(is.null(fit.machine())){return(NULL)}
                    if(input$plot_group=="ROC.train"|input$plot_group=="ROC.test"){
                      if(input$plot_group=="ROC.train"){ # data train ROC plot
                        roc_list <- roc(group~pred , direction='auto' ,
                                        ci=TRUE , aur=TRUE , percent=F ,
                                        data = df_train() )  }
                      else if(input$plot_group=="ROC.test"){  # data test ROC plot
                        roc_list <- roc(group~pred , direction='auto' ,
                                        ci=TRUE , aur=TRUE , percent=F ,
                                        data = df_test() )
                      }
                      
                      data.auc <- roc_list %>% ci() %>% 
                        data.frame() %>% t()  %>%
                        as.data.frame() %>% round( digits = 3)
                      colnames(data.auc) <- c("ci_lower","auc","ci_upper")
                      data.auc$name <- paste(input$age_and_genes, collapse =" + ")
                      
                      best.point <- roc_list %>% 
                        coords("best", ret=c("threshold", "sensitivity","1-specificity", "npv","ppv"))
                      best.point <- best.point %>%  round(digits = 3) %>% data.frame()
                      colnames(best.point) <- c("threshold", "sensitivity","1-specificity", "npv","ppv")
                      best.point$name <- paste(input$age_and_genes, collapse =" + ")
                      
                      p <- ggroc(roc_list, legacy.axes = T,
                                 linetype = input$linetype,
                                 color    = input$linecolor, 
                                 linewidth= input$linesize) +
                        geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
                                     linetype = input$linetype, 
                                     color    = input$linecolor,
                                     linewidth= input$linesize, 
                                     show.legend= F)
                      
                      if(input$auc=="show"){
                        p <- p+
                          geom_text(data.auc, hjust = 0.5,
                                    show.legend= F,
                                    size= input$auc_size, 
                                    color   = input$textcolor,
                                    mapping= aes(0.6, 0.4, 
                                                 label = paste("AUC=",auc,"\n","95% CI: ",ci_lower,"~",ci_upper,
                                                               "\n",paste(input$age_and_genes, collapse =" + ") ) ) )
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
                    }
                    else if(input$plot_group=="Dtree"){
                      library(ggparty)
                      fit.machine <- fit.machine() # partykit::ctree(group~., data= df[,-2] )
                      pvals <- unlist(nodeapply(fit.machine, ids = nodeids(fit.machine), function(n) info_node(n)$p.value))
                      pvals <- pvals[pvals <.05]
                      
                      p <- ggparty(fit.machine) +
                        geom_edge() +
                        geom_edge_label() +
                        geom_node_label(line_list = list(aes(label = splitvar,fill = group,color=group),
                                                         aes(label = paste0("N=", nodesize, ", p", 
                                                                            ifelse(pvals < .001, "<.001", paste0("=", round(pvals, 3)))), 
                                                             size = 10)),
                                        line_gpar = list(list(size = 13), 
                                                         list(size = 10)), 
                                        ids = "inner") +
                        geom_node_plot(gglist = list(
                          geom_bar(aes(x = "", fill = group,color=group),
                                   position = position_fill(), color = "white"),
                          eval(parse(text = paste0("theme_",input$theme,"()"))),
                          eval(parse(text = paste0("scale_fill_",input$color_type,"()"))),
                          eval(parse(text = paste0("labs(fill='",input$color_name,"')") )),
                          scale_y_continuous(breaks = seq(0,1,0.2) ),
                          xlab(input$x.axis.title), 
                          ylab( ifelse(input$y.axis.title=="","Probability",input$y.axis.title) ),
                          # theme_plot.title(input),
                          theme_axis.title(input), 
                          theme_axis.text(input),
                          theme_axis.ticks(input),
                          theme_legend.title(input),
                          theme_legend.text(input)
                        ),
                        shared_axis_labels = TRUE) +
                        geom_node_label(aes(label = paste0("Node ", id, ", N = ", nodesize)),
                                        ids = "terminal") 
                      ifelse(!input$title=='', 
                             p <- p+ggtitle(input$title), 
                             p <- p+ggtitle("Condition Inference Tree") )
                      
                      p <- p + theme_plot.title(input)
                      
                      return(p)
                    } 
                  })
                  
                  output$plot <- renderPlot( plot() )
                  
                  # download plot: pdf, png, jpeg, tiff, rds
                  output$pdf0  <- eval(parse(text = output_plot[1] ))
                  output$png0  <- eval(parse(text = output_plot[2] ))
                  output$jpeg0 <- eval(parse(text = output_plot[3] ))
                  output$tiff0 <- eval(parse(text = output_plot[4] ))
                  output$rds0  <- eval(parse(text = output_plot[5] ))
                  
                  output$df_pref.train<- DT::renderDataTable( df_pref.train())
                  output$table_pref.train  <- downloadHandler(filename="pref.train.csv",
                                                              content = function(file){write.csv( df_pref.train() , file, row.names = T ) }  )
                  output$df_train <- DT::renderDataTable( df_train())
                  output$table_train <- downloadHandler(filename="data.train.csv",
                                                        content = function(file){write.csv(df_train() , file, row.names = T ) }  )
                  
                  output$df_pref.test <- DT::renderDataTable( df_pref.test() )
                  output$table_pref.test  <- downloadHandler(filename="pref.test.csv",
                                                             content = function(file){write.csv( df_pref.test() , file, row.names = T ) }  )
                  output$df_test  <- DT::renderDataTable( df_test() )
                  output$table_test  <- downloadHandler(filename="data.test.csv",
                                                        content = function(file){write.csv(df_test() , file, row.names = T ) }  )
                  
                } )
                
              })
              
            })
            
          })
          
        }
        
      })
      
    } ) }
for(i in dbListConnections(MySQL()) ){dbDisconnect(i)}