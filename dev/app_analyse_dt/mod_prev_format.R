analyse_dt_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput("dt"),
    # Select how to display the prevalence values
    selectInput(
      ns("optsPrevFormat"),
      "Prevalence output format",
      choices = c("Prevalence per value", "Raw prevalence")
    ),
    conditionalPanel(
      condition = "input.optsPrevFormat == 'Prevalence per value'",
      numericInput("optsPerVal", "Prevalence per value")
    )
    numericInput(ns("optsRounding")),

  )
}

analyse_dt_server <- function(id) {
  moduleServer(id, function(id, input, session) {

    #
    output$optsPrevFormatUI <- renderUI({
      tagList(

      )
    })

  })

}
