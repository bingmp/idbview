
# library('shiny')
library('tidyr')
# library('DT')
library('ggplot2')
# library('shinydashboard')
library('forecast') # time series
library('lubridate') # for showing up time correctly
library('scales')
library('nCov2019') # remotes::install_github("GuangchuangYu/nCov2019")
# library('chinamap') # remotes::install_github("GuangchuangYu/chinamap") 

server <- function(input, output, session, ...) {
  
  # load data 
  data <- readRDS('www/nCov2019_data.RDS')  # data = nCov2019::load_nCov2019(lang='en')
  
  # update date
  observe({
    updateSelectizeInput(session, 'date_t', choices = data$time, server = TRUE)
  })
  
  observe({
  # observeEvent(input$showData,{
    
    # update date
    t <- input$date_t
    
    # update country list
    country_list <- filter(data$global, time == t) %>% 
      arrange(desc(cum_confirm)) %>% .$country
    updateSelectizeInput(session, 'country', choices = country_list,selected = 'China', server = TRUE)

    # update province list
    observe({
      province <- unique(subset(data$province, country == input$country)$province)
      updateSelectInput(session, "province", choices = c("",province))
    })
    
    # update city list
    observe({
      city <- unique(subset(data$data, province == input$province)$city)
      updateSelectInput(session, "city", choices = c("",city))
    })
    
    # prepare the table content
    df <- reactive({
      x = data.frame()
      if ( nchar(input$country) > 0 ) {
        x = subset(data$global, country == input$country)
      }
      if ( nchar(input$province) > 0 ) {
        x = subset(data$province, province == input$province)
      }
      if ( nchar(input$city) > 0  ) {
        x =  subset(data$data, city == input$city)
      }
      x = x[,c("time","cum_confirm","cum_heal","cum_dead")]
      return(x)
    })
    
    num <- reactive({
      input$num
    })
    
    
    # output data table
    output$data_table = DT::renderDataTable({
      validate(need(input$country != "", "Loading"))
      df()
    },rownames = FALSE )
    
    
    # output header summary 
    output$summary_confirm <- renderValueBox({
      validate(need(input$country != "", "Loading"))
      x = df()
      valueBox(
        paste0(x[which(x$time == t),]$cum_confirm, " confirm"), 
        t, icon = icon("virus"), color = "yellow")
    })
    
    output$summary_cure <- renderValueBox({
      validate(need(input$country != "", "Loading"))
      x = df()
      valueBox(
        paste0(x[which(x$time == t),]$cum_heal, " health"), 
        t, icon = icon("hospital"), color = "green")
    })
    
    output$summary_dead <- renderValueBox({
      validate(need(input$country != "", "Loading"))
      x = df()
      valueBox(
        paste0(x[which(x$time == t),]$cum_dead, " dead"), 
        t, icon = icon("skull-crossbones"), color = "red")
    })
    
    output$line_plot <- renderPlotly({
      validate(need(input$country != "", "Loading"))
      x = gather(df(), curve, count, -time)
      p = ggplot(x, aes(time, count, color = curve)) +
        geom_point() + geom_line() + xlab(NULL) + ylab(NULL) +
        scale_color_manual(values=c("#f39c12", "#dd4b39", "#00a65a")) +
        theme_bw() + 
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
        scale_x_date(date_labels = "%Y-%m-%d")
      ggplotly(p)
    })
    
    # data download
    output$dataDownload <- downloadHandler(
      filename = function() {paste0("coronavirus_histrical_",t,".tsv")},
      content = function(file) {
        # issues with Chinese characters solved
        # http://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
        con <- file(file, open = "w+", encoding = "native.enc")
        df <- df()
        df$country = input$country
        df$province = input$province
        df$city = input$city
        df$time <- as.character(format(df$time))
        writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
        for(i in 1:nrow( df) )
          #write line by line 
          writeLines( paste( df[i,], collapse = "\t"), con = con, useBytes = TRUE)
        close(con)
      }
    )
    
    # bottom panel plots
    
    # output worldwide_plot
    output$worldwide_plot <- renderPlot({
      validate(need(input$country != "", "Loading"))
      plot(data,date=t)
    })
    
    # output country_plot
    output$country_plot <- renderPlot({
      validate(need(input$country != "", "Loading"))
      country_eng = input$country
      country_eng <- sub("United\\sStates.*", "USA", country_eng)
      country_eng <- sub("Republic\\sof\\sKorea", "South Korea", country_eng)
      country_eng <- sub("United\\sKingdom.*", "UK", country_eng)
      country_eng <- sub("Republika\\sSeverna\\sMakedonija", "Macedonia", country_eng)
      # if(country_eng == 'China'){country_eng = c('China','Taiwan')}
      if(country_eng == 'China'){
        cn <- chinamap::get_map_china()
        cn$province <- nCov2019::trans_province(cn$province)
        plot(data, region = 'china', chinamap = cn,continuous_scale = FALSE,date=t)
      }else{
        plot(data, region = country_eng, date=t)
      }
      
      
    })
    
    # top country plots
    output$Mortality_plot <- renderPlotly({
      
      d = data$global
      df <- filter(d, time == t) %>% 
        arrange(desc(cum_confirm)) 
      df = df[1:100, ]
      
      df$rate = df$cum_dead/df$cum_confirm
      
      df <- df %>% 
        arrange(desc(rate)) 
      
      df$country = factor(df$country, levels=df$country)
      percent <- function(x, digits = 2, format = "f", ...) {
        paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
      }
      
      df$Mortality <- percent(df$rate)
      df2 = df[order(df$cum_dead,decreasing = T),][1:30,]
      
      p <- ggplot(df, aes(x = country, y = rate, color = rate ,label = Mortality, confirm = cum_confirm )) +
        geom_point(aes(size=cum_confirm), alpha = .65) + 
        scale_color_gradientn(colors=c("darkgreen", "orange", "firebrick","red")) +
        labs(title = "COVID-19 Mortality Rate") + theme_bw() + 
        theme(legend.position = "none") + xlab(NULL) + ylab('Mortality Rate') + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
        scale_size(range=c(1,20)) + theme(axis.text.x = element_text(angle=45, hjust=1))
      
      ggplotly(p,tooltip = c("x","label","confirm"))
      
    })
    
    # top country plots
    output$Health_plot <- renderPlotly({
      
      d = data$global
      df <- filter(d, time == t) %>% 
        arrange(desc(cum_confirm)) 
      df = df[1:100, ]
      
      df$rate = df$cum_heal/df$cum_confirm
      df <- df %>% 
        arrange(desc(rate)) 
      
      df$country = factor(df$country, levels=df$country)
      percent <- function(x, digits = 2, format = "f", ...) {
        paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
      }
      
      df$Health <- percent(df$rate)
      
      p <- ggplot(df, aes(x = country, y = rate, color = rate ,label = Health, Health = cum_heal )) +
        geom_point(aes(size=cum_heal), alpha = .65) + 
        scale_color_gradientn(colors=c("firebrick","orange","darkgreen","green")) +
        labs(title = "COVID-19 Health Rate") + theme_bw() + 
        theme(legend.position = "none") + xlab(NULL) + ylab('Health Rate') + 
        scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
        scale_size(range=c(1,20)) + theme(axis.text.x = element_text(angle=45, hjust=1))
      ggplotly(p,tooltip = c("x","label","Health"))
    })
    
    output$forecast <- renderPlot ({
      d2 <- df()
      options(scipen=999)
      options(warn=-1)
      par(mar = c(4, 3, 0, 2))
      smooth_confirm = stats::filter(d2$cum_confirm, rep(1/10,10), sides=1 )
      #smooth_heal = stats::filter(d2$cum_heal, rep(1/10,10), sides=1 )
      #smooth_dead = stats::filter(d2$cum_dead, rep(1/10,10), sides=1 )
      confirm <- smooth_confirm %>%
        ets() %>%
        forecast(num())
      plot(confirm, xaxt="n", main="") + ylim(0,NA)
      
    }) 
    
    output$growth_rate <- renderPlotly ({
      d2 <- data$global
      smooth_confirm = stats::filter(d2$cum_confirm, rep(1/10,10), sides=1 )
      #seq(from =  nrow(J), to = 1,-7) %>% rev -> idx
      # plot(confirm, xaxt="n", main="") + ylim(0,NA)
      diff_rate <- function(x){
        return(diff(x)/x[1])
      }
      
      d2 %>% na.omit() %>% group_by(country) %>% 
        filter(as.Date(t) %in% time & (as.Date(t)-1) %in% time) %>% 
        filter(time == as.Date(t)|time == (as.Date(t)-1)) %>% 
        mutate(growth_rate = round(diff_rate(cum_confirm),3)) %>% 
        ungroup() -> d3
      
      dd <- filter(d3, time == t) %>% 
        arrange(desc(cum_confirm)) %>% .[1:100, ] %>%
        arrange(desc(growth_rate)) 
      dd$country = factor(dd$country, levels=dd$country)
      
      percent <- function(x, digits = 2, format = "f", ...) {
        paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
      }
      
      dd$rate <- percent(dd$growth_rate)
      p = ggplot(dd,aes(x=country,y= growth_rate,color = growth_rate,size = cum_confirm, label = rate, alpha = .6)) + geom_point() +
        scale_color_gradientn(colors=c('green',"darkgreen","orange","firebrick","red")) + 
        scale_size_continuous() + guides(alpha = F) +
        theme_bw() + scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
        theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) + labs(title = "Current Growth Rate")
      ggplotly(p,tooltip = c("x","label","size"))
    })
    ### end
  })

}
