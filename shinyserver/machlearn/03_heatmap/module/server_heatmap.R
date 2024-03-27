
library('RColorBrewer')
library('pheatmap')
source('module/server_global.R')

heatmapServer <- function(id) {
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
          
          observe({  
            df <- df()
            updateSelectInput(session,"group_select" , choices = unique(df$group) ,
                              selected = unique(df$group)  )
            updateSelectInput(session,"var_select" , choices = unique(df$var) ,
                              selected = unique(df$var))
            updateSelectInput(session,"age_and_genes" , choices = colnames(df)[3:ncol(df)] ,
                              selected = colnames(df)[-(1:3)] )
            
            observe({
              if(!is.null( input$age_and_genes ) ){
                df <- df() %>%  dplyr::select(c("group","var",input$age_and_genes)) 
              }else{
                df <- df()
              }
              df <- df[order(df[,input$plot_group]),] %>%
                subset(eval(parse(text = paste('group'," =='", input$group_select ,"'",sep = '', collapse = '|'))) ) %>%
                subset(eval(parse(text = paste('var'," =='", input$var_select ,"'",sep = '', collapse = '|'))) )
              df$group <- factor(df$group,levels = input$group_select)
              df$var <- factor(df$var,levels = input$var_select)
              
              observeEvent(input$submit, {
                
                anno_col <- reactive({
                  if(input$plot_group=="group"){
                    anno_col <- data.frame(group=df$group )
                    colnames(anno_col) <- c(input$group_name)
                    
                  }else if(input$plot_group=="var"){
                    anno_col <- data.frame(group=df$var )
                    colnames(anno_col) <- c(input$var_name)
                  }
                  else{
                    anno_col <- df[,c("group","var")]
                    colnames(anno_col) <- c(input$group_name,input$var_name)
                  }
                  rownames(anno_col) <- rownames( df )
                  
                  return(anno_col )
                } )
                
                # output$anno_col <- DT::renderDataTable( anno_col()   )
                
                plot <- reactive({
                  if(is.null(input$age_and_genes)|length(input$age_and_genes)==1 ){return(NULL)}
                  
                  if(input$data_log ){
                    d <- df %>%  dplyr::select(-'group', -'var') %>% apply( MARGIN = 2, FUN = function(x){log2(x+1)}) %>% t()
                  }else{
                    d <- df %>%  dplyr::select(-'group', -'var' ) %>% t()
                  }
                  
                  p <-  pheatmap(d, fontsize    = 15, fontsize_row=15, fontsize_col = 20,
                                 cluster_cols   = c(input$col_cluster=='T'),
                                 cluster_rows   = c(input$row_cluster=='T'),
                                 show_colnames  = c( input$colname=='T'),
                                 show_rownames  = c(input$rowname=='T'),
                                 annotation_col = anno_col(),
                                 scale = input$scale,
                                 display_numbers = c(input$disp_num=='T'),
                                 angle_col = as.numeric(input$angle_col),
                                 color = colorRampPalette(c(input$color1,input$color2, input$color3))(50)
                  )
                  
                  
                  return(p)
                }) 
                
                
                output$plot <- renderPlot({
                  return(plot() )
                })
                
                # download plot: pdf, png, jpeg, tiff, rds
                output$pdf0  <- eval(parse(text = output_plot[1] ))
                output$png0  <- eval(parse(text = output_plot[2] ))
                output$jpeg0 <- eval(parse(text = output_plot[3] ))
                output$tiff0 <- eval(parse(text = output_plot[4] ))
                output$rds0  <- eval(parse(text = output_plot[5] ))
                
              })
              
            })
          })
          
        }
        
      }) # observeEvent(input$show, 
    } # function(input, output, session) 
  ) # moduleServer
} # function(id) 
for(i in dbListConnections(MySQL()) ){dbDisconnect(i)}
