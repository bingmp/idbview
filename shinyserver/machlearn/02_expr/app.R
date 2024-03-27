
library('shiny') 
library('bs4Dash')
library('shinyWidgets')
# library('shiny.i18n')

source('module/ui_expr.R')
# File with translations
lang <- shiny.i18n::Translator$new(translation_csvs_path = "./lang/info/")
lang$set_translation_language("en")

ui <- bs4DashPage(
  fullscreen = T,
  header = bs4DashNavbar(
    shiny.i18n::usei18n(lang),
    column(width = 2,br(),
           shinyWidgets::materialSwitch(inputId = "lang",label = lang$t("English"),
                          status = "primary",value = F, right = T )
    ),
    disable = FALSE, skin = "light",  status = "white",  border = T, fixed = T
  ),
  ## Sidebar content
  sidebar = bs4DashSidebar(disable = T ),
  
  body  = dashboardBody(
    exprUI("expr")
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$lang, {
    ifelse(input$lang,
           shiny.i18n::update_lang('cn' ),
           shiny.i18n::update_lang('en' ) )
  })

  source('module/server_expr.R')
  exprServer("expr")
}

shinyApp(ui, server)
