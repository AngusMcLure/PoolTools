library(shiny)

ui <- fluidPage(
  titlePanel("PoolTools"),

  ## Main navbar and pages
  navbarPage("Navigation", id = "main_nav",
             tabPanel("Home",
                      wellPanel(
                        h4("Select:"),
                        actionButton("btn_help", "Help (Documentation)"),
                        actionButton("btn_analyse", "Analyse pooled data"),
                        actionButton("btn_design", "Design a pooled survey")
                      )
             ),
             tabPanel("Documentation",
                      h3("Documentation")
             ),
             tabPanel("Analyse",
                      h3("Analyse pooled data"),
                      fileInput("file_analyse", "Choose CSV File")
             ),
             tabPanel("Design",
                      h3("Design a pooled survey")
             )
  )
)

server <- function(input, output, session) {

  ## Home page buttons
  observeEvent(input$btn_help, {
    updateTabsetPanel(session, "main_nav", selected = "Documentation")
  })

  observeEvent(input$btn_analyse, {
    updateTabsetPanel(session, "main_nav", selected = "Analyse")
  })

  observeEvent(input$btn_design, {
    updateTabsetPanel(session, "main_nav", selected = "Design")
  })

}

shinyApp(ui = ui, server = server)
