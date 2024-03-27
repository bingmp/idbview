
source('module/server_global.R')
datasetServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      dataset <- reactive({
        con <- eval(parse(text = mysql_con))
        dataset <- dbGetQuery(con, 'SELECT * FROM all_dataset' )
        dbDisconnect(con)
        return( dataset )
      })
      output$dataset <- DT::renderDataTable( dataset())
      
      observe({
        updateSelectInput(session,"choose_gse" , choices = paste(dataset()$gse,dataset()$type,sep = '_') ,
                          selected = paste(dataset()$gse,dataset()$type,sep = '_')[1] )
      })

      observeEvent(input$show_gse, {
          gse <- reactive({
            con <- eval(parse(text = mysql_con))
            gse <- dbGetQuery(con, paste("SELECT * FROM all_group WHERE gse =","'",
                                         strsplit(input$choose_gse,split = '_')[[1]][1],
                                         "'",sep = '') )
            dbDisconnect(con)
            return( gse )
          })
          updateSelectInput(session,"choose_condition" , choices = unique(gse()$condition) ,
                            selected = unique(gse()$condition))
          updateSelectInput(session,"choose_cluster" , choices = unique(gse()$cluster) ,
                            selected = unique(gse()$cluster))
          updateSelectInput(session,"choose_sample" , choices = unique(gse()$sample) ,
                            selected = unique(gse()$sample) )
          updateSelectInput(session,"choose_gender" , choices = unique(gse()$gender) ,
                            selected = unique(gse()$gender))
          
            observeEvent(input$show_gse, { 
              gse_group <- reactive({
                
                gse_group <- gse() %>%
                  subset(eval(parse(text = paste("condition =='", input$choose_condition ,"'",sep = '', collapse = '|'))) ) %>%
                  subset( eval(parse(text = paste("cluster =='",input$choose_cluster ,"'",sep = '', collapse = '|'))) ) %>%
                  subset( eval(parse(text = paste("sample =='",input$choose_sample ,"'",sep = '', collapse = '|'))) ) %>%
                  subset(eval(parse(text = paste("gender =='",input$choose_gender ,"'",sep = '', collapse = '|'))) ) %>%
                  subset(age>=input$choose_age_min & age<=input$choose_age_max )
                
                return( gse_group )
              })
            output$gse_group <- DT::renderDataTable( gse_group())
            
              con <- eval(parse(text = mysql_con))
              gene_ID <- dbGetQuery(con, paste0("select ID from ",input$choose_gse,"_exp" ) )[,1]
              updateSelectizeInput(session,"choose_gene" , choices = gene_ID ,server = TRUE,
                                   selected = # gene_ID[1:2] 
                                     c('CCL26', 'CDC42EP5', 'CSTA', 'MUC5B','PHLDB2', 'POSTN','SERPINB2')
                                     # c("ACSL1", "CD82","DPP4","GALNT14","PPARG", "WIPI1")
                                   )
              dbDisconnect(con)
          observe({
              contrast_group <- unique( gse_group()[,input$choose_learning_column] )
              updateSelectInput(session,"choose_con" , 
                                choices  = contrast_group ,
                                selected = contrast_group[1] )
              updateSelectInput(session,"choose_case" , 
                                choices  = contrast_group ,
                                selected = contrast_group[2] )
            
                
             observeEvent(input$show_learning_data, { 
               
              gse_exp <- reactive({

               # if(is.null(input$choose_con)|is.null(input$choose_case)){return(NULL)}
              
                group_untrt <- gse_group() %>% 
                  subset(eval(parse(text = paste(input$choose_learning_column," =='", input$choose_con ,"'",sep = '', collapse = '|'))) )
                if( nrow(group_untrt)!=0 ){ group_untrt$contrast <- input$con_name }

                group_trt <- gse_group() %>% 
                  subset(eval(parse(text = paste(input$choose_learning_column," =='", input$choose_case ,"'",sep = '', collapse = '|'))) ) 
                if( nrow(group_trt)!=0 ){ group_trt$contrast <-  input$case_name }

                mygroup <- rbind(group_untrt, group_trt)
                if( nrow(mygroup)==0 ){return(NULL)}
                
                con <- eval(parse(text = mysql_con))
                mydata <- dbGetQuery(con, paste0("select * from ",input$choose_gse,"_exp where ",
                                                  "ID in ","(",
                                                  paste("'",input$choose_gene,"'" ,sep = '', collapse = ','),
                                                  ") "
                                                  )
                                      )[,c("ID",mygroup$gsm )]
                dbDisconnect(con)
                # if( nrow(mydata)==0 ){return(NULL)}
                rownames(mydata) <- mydata$ID ;  mydata <- mydata[,-1]
                
                gse_exp <- cbind( data.frame(group = mygroup$contrast,
                                             var   = mygroup[,input$choose_learning_column],
                                             age   = mygroup$age ) ,
                                             as.data.frame( t( mydata ) )
                )
                
                return(gse_exp)
              })
              output$gse_exp <- DT::renderDataTable( gse_exp())
              
              if(T){
     
              output$download_gene_csv <- downloadHandler(
                filename = paste(input$choose_gse,"data.csv",sep = '_'),
                content  = function(file){ write.csv(gse_exp(), file, row.names = T) }
                )
              output$download_gene_rds <- downloadHandler( 
                filename = paste(input$choose_gse,"data.rds",sep = '_'),
                content  = function(file){ saveRDS( gse_exp() ,file)
                }  )
              }
              
              observeEvent(input$submit_data, { 
                con <- eval(parse(text = mysql_con))
                dbWriteTable(con,"select_data", gse_exp(),row.names=T,overwrite=TRUE) # 覆盖
                dbListTables(con)
              })

              }) # show_learning_data: choose gene data
             
            })

            }) # show_gse: choose gse dataset

        }) # 

       observeEvent(input$remove_data, { 
        con <- eval(parse(text = mysql_con))
        dbRemoveTable(con,"select_data")  # 删除表
        dbListTables(con)
      }) 
      
    } ) }
for(i in dbListConnections(MySQL()) ){dbDisconnect(i)}
