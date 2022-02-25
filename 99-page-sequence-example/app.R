library(shiny)
library(shinyjs)

ui <- navbarPage('Test App', id = "inTabset",
                 tabPanel(title = "Panel 1", value = "panel1", 
                          actionButton('jumpToP2', 'Jump to Secon Tab')),
                 tabPanel(title = "Panel 2", value = "panel2", 
                          actionButton('jumpToP1', 'Jump to First Tab')),
                 useShinyjs(),
                 tags$head(tags$style(HTML('.navbar-nav a {cursor: default}')))
)

server <- function(input, output, session) {
  
  shinyjs::disable(selector = '.navbar-nav a')
  
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
}

shinyApp(ui, server)