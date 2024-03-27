
machine_table_UI <- function(id){# download table ui setting
  ns <- NS(id)
  tagList(
    splitLayout(cellWidths = c("50%","50%"),height = 600 ,  
                DT::dataTableOutput(ns("df_pref.train") ),
                DT::dataTableOutput(ns("df_pref.test") ) ),
    hr(),
    splitLayout(cellWidths = c("50%","50%"),height = 600,
                DT::dataTableOutput(ns("df_train") ),
                DT::dataTableOutput(ns("df_test") )  )
  )
}