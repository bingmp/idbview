
source('module/server_global.R')
exprServer <- function(id) {
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
            updateSelectInput(session,"group_select" , choices = unique(df$group) ,
                              selected = unique(df$group)  )
            updateSelectInput(session,"var_select" , choices = unique(df$var) ,
                              selected = unique(df$var))
            updateSelectInput(session,"age_and_genes" , choices = colnames(df)[3:ncol(df)] ,
                              selected = colnames(df)[4:ncol(df)] )
            
            observe({
              if(!is.null( input$age_and_genes ) ){
                df <- df() %>%  dplyr::select(c("group","var",input$age_and_genes)) 
              }else{
                df <- df()
              }
              df <- df %>%
                subset(eval(parse(text = paste('group'," =='", input$group_select ,"'",sep = '', collapse = '|'))) ) %>%
                subset(eval(parse(text = paste('var'," =='", input$var_select ,"'",sep = '', collapse = '|'))) )
              # df$group <- factor(df$group,levels = input$group_select)
              # df$var <- factor(df$var,levels = input$var_select)
              
            observeEvent(input$submit, { 

              # gene rexpression boxplot
              plot <- reactive({
                df$group <- factor(df$group,levels = input$group_select)
                df$var <- factor(df$var,levels = input$var_select)

                df_long <- reactive({
                  df_long <- reshape2::melt( df , id.vars = c("group","var"),
                                             measure.vars = setdiff(colnames(df),c("group","var")),
                                             variable.name = c('gene'),
                                             value.name = 'value')
                  return( df_long )
                } )
                
                ifelse(input$plot_group=="group",
                       p <- ggplot(df_long() ,aes(x=gene , y=value,color=group ) ) ,
                       p <- ggplot(df_long() ,aes(x=gene , y=value,color=var ) ) 
                       )
                p <- p+
                  geom_boxplot()  + 
                  geom_jitter( size=0.1,show.legend = F )
                
                if(!input$method=='none'){ #  Add p-value
                  ifelse(input$label.x==0 | input$label.y==0,
                         p <- p+ stat_compare_means(method = input$method,size=8),
                         p <- p+ stat_compare_means(method = input$method,size=8,
                                                    label.x = input$label.x, 
                                                    label.y = input$label.y)
                         )
                }
                
                p <- ggplot_plot(p,input)+
                  theme_axis.title(input) +
                  theme_axis.text(input) +
                  theme_axis.ticks(input) +
                  theme_legend.title(input) +
                  theme_legend.text(input)

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