
library('ggplot2')
library("ggsci")
library('ggpubr')

ggplot_plot <- function(p,input){
  if(!input$theme==''){
    p <- p + eval(parse(text = paste0("theme_",input$theme,"()")))
  }
  if(!input$color_type==''){
    p <- p + eval(parse(text = paste0("scale_color_",input$color_type,"()")))
  }
  if(!input$color_name==''){
    p <- p + eval(parse(text = paste0("labs(color='",input$color_name,"')") ))
  }
  
  if(!input$title==''){
    p <- p + ggtitle( input$title )
  }
  if(!input$x.axis.title==''){
    p <- p + xlab( input$x.axis.title )
  }
  if(!input$y.axis.title==''){
    p <- p + ylab( input$y.axis.title )
  }
  return(p)
}
theme_plot.title   <- function(input){ # 标题设置
  theme(
    plot.title   = element_text(size  = input$size_title,
                                hjust = input$hjust_title,
                                color = input$color_title,
                                vjust = input$vjust_title,
                                angle = input$angle_title
    )
  )
}
theme_axis.title   <- function(input){ # 坐标轴标题
  theme(
    axis.title   = element_text(size  = input$size_axis.title,
                                color = input$color_axis.title,
                                hjust = input$hjust_axis.title,
                                vjust = input$vjust_axis.title,
                                angle = input$angle_axis.title
    )
  )
}
theme_axis.text    <- function(input){ # 坐标轴标签
  theme(
    axis.text    = element_text(size  = input$size_axis.text,
                                color = input$color_axis.text,
                                hjust = input$hjust_axis.text,
                                vjust = input$vjust_axis.text,
                                angle = input$angle_axis.text
    )
  )
}
theme_axis.ticks   <- function(input){ # 坐标轴标签
  theme(
    axis.ticks   = element_line(linewidth = input$size_axis.ticks,
                                linetype  = input$linetype_axis.ticks,
                                color     = input$color_axis.ticks
    )
  )
}
theme_legend.title <- function(input){ # 图例标签
  theme(
    legend.title = element_text(size  = input$size_legend.title,
                                hjust = input$hjust_legend.title,
                                color = input$color_legend.title,
                                vjust = input$vjust_legend.title,
                                angle = input$angle_legend.title
    )
  )
}
theme_legend.text  <- function(input){# 图例文字
  theme(
    legend.text  = element_text(size  = input$size_legend.text,
                                hjust = input$hjust_legend.text,
                                color = input$color_legend.text,
                                vjust = input$vjust_legend.text,
                                angle = input$angle_legend.text
    )
  )
}
