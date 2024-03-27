
# library('RColorBrewer')
source('module/server_global.R')

svmServer <- function(id) {
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
              
              observe({
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
                
                observeEvent(input$submit, {
                  
                  df.train <- df.train()
                  df.test  <- df.test()
                  output$sample_size <- renderText({
                    paste("train: ",nrow(df.train),";", "test: ", nrow(df.test) )
                  })
                  
                  library(e1071)
                  fit.machine <- reactive({
                    if(nrow(df.train())==0){return(NULL)}
                    if(is.na(input$cost_svm) | is.na(input$gamma_svm) ){
                      if(!is.na(input$set_seed) ){set.seed(input$set_seed)}
                      fit.machine <- svm(group~.,
                                         na.action =na.omit ,
                                         data = df.train[,-2])
                    }
                    else{
                      if(!is.na(input$set_seed) ){set.seed(input$set_seed)}
                      fit.machine <- svm(group~.,
                                         na.action =na.omit,
                                         gamma= input$gamma_svm,
                                         cost= input$cost_svm, 
                                         data = df.train[,-2])
                    }
                    
                    return(fit.machine)
                  })
                  output$gamma_cost <- renderText({ paste("cost: ",round(fit.machine()$cost,3),";  gamma: ",round( fit.machine()$gamma,3) ) })
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
                    df_train$pred_group <- predict(fit.machine(), df_train[,-2] )
                    df_train <- df_train[,c(1,ncol(df_train),c(3:ncol(df_train)-1))]
                    return(df_train) 
                  })
                  df_test  <- reactive({
                    if(is.null(fit.machine())){return(NULL)}
                    df_test <- df.test
                    df_test$pred_group <- predict(fit.machine(), df_test[,-2] )
                    df_test <- df_test[,c(1,ncol(df_test),c(3:ncol(df_test)-1))]
                    return(df_test)
                  })
                  
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
                  
                })
                
                observeEvent(input$submit_svm,{
                  df.train <- df.train()
                  df.test  <- df.test()
                  library(e1071)
                  fit.tuned <- reactive({
                    if(nrow(df.train())==0){return(NULL)}
                    if(!is.na(input$set_seed) ){set.seed(input$set_seed)}
                    fit.tuned <- tune.svm(group ~., 
                                          gamma = seq(0.1,1,0.2),
                                          cost = seq(0.1,5,1),
                                          na.action =na.omit,
                                          data = df.train[,-2])
                    return(fit.tuned)
                  })
                  output$gamma_recmmond <- renderText({ paste("gamma recmmond: ", fit.tuned()$best.parameters$gamma ) })
                  output$cost_recmmond <- renderText({ paste("cost recmmond: ", fit.tuned()$best.parameters$cost ) })
                  
                  plot <- reactive({
                    if(is.null(fit.tuned() ) ){return(NULL)}
                    plotdata <- fit.tuned()$performances
                    head(plotdata) ## gamma cost error dispersion
                    
                    p <- ggplot(plotdata,aes(x = cost, y = gamma))+
                      geom_tile(aes(fill = error))+
                      scale_fill_gradientn(colours= RColorBrewer::brewer.pal(9,"OrRd"))+
                      ggtitle("Performance of SVM")
                    
                    p <- ggplot_plot(p,input)+
                      theme_axis.title(input) +
                      theme_axis.text(input) +
                      theme_axis.ticks(input) +
                      theme_legend.title(input) +
                      theme_legend.text(input)
                    
                    return(p)
                    
                  })
                  output$plot <- renderPlot( plot() )
                  
                  
                })
                
              })
            })
          })
        }
      })
    } ) }
for(i in dbListConnections(MySQL()) ){dbDisconnect(i)}