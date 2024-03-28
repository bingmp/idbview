
download_table_UI <- function(id){# download table ui setting
  ns <- NS(id)
  tagList(
    # box(width=3,status="success",
    # actionBttn( inputId = ns("submit_table"), label = lang$t("Analyze Data"),
    #             style = "fill", color = "primary", size = "sm" ),hr(),
    dropdownButton(circle=FALSE, label = lang$t("download table"), status="success",icon = icon("download"),
                   br(),br() ,
                   downloadBttn(outputId = ns("fit.machine"), label = "fit.machine", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("table_pref.train"),  label = "pref.train", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("table_pref.test") ,  label = "pref.test", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("table_train") ,  label = "data.train", size='sm', block=TRUE ),
                   downloadBttn(outputId = ns("table_test") ,  label = "data.test", size='sm', block=TRUE ) 
                   # )
    )
  )
}
