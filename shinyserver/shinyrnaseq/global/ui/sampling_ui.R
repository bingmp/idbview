
sampling_UI <- function(id) { # sampling ui
  ns <- NS(id)
  fluidRow(
    column(width = 12, span(textOutput(ns("sample_size")), style="color:orange") ),
    column(width = 12, sliderInput(ns("size_train"),     label = lang$t('data.train size (70%)'),value =2,min = 2,max = 10000) ),
    # column(width = 12, sliderInput(ns("size_test"),      label = 'data.test size (30%)', value =2,min = 2,max = 10000) ),
    column(width = 12, numericInput(ns("set_seed"),      label = lang$t('set.seed'),value = NA ) ) 
  )
}
