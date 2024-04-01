
output_plot <- c(
  paste("downloadHandler(filename='plot.",
        "pdf","',content=function(file){",
        "pdf","(file",
        ",width=input$w0,height=input$h0);",
        "print(plot() );dev.off() })",sep = '') ,
  paste("downloadHandler(filename='plot.",
        "png","',content=function(file){",
        "png","(file,units='in',res=input$ppi0",
        ",width=input$w0,height=input$h0);",
        "print(plot() );dev.off() })",sep = '') ,
  paste("downloadHandler(filename='plot.",
        "jpeg","',content=function(file){",
        "jpeg","(file,units='in',res=input$ppi0",
        ",width=input$w0,height=input$h0);",
        "print(plot() );dev.off() })",sep = '') ,
  paste("downloadHandler(filename='plot.",
        "tiff","',content=function(file){",
        "tiff","(file,units='in',res=input$ppi0",
        ",width=input$w0,height=input$h0);",
        "print(plot() );dev.off() })",sep = '') ,
  "downloadHandler(filename='plot.RDS',content=function(file){saveRDS(plot(),file)})"
)

