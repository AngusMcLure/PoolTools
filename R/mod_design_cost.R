cost_ui <- function(id, r_store) {
  ns <- NS(id)
  numericInput(ns("cost"), paste0("Cost per ", id), value = isolate(r_store[[id]]), min = 1e-6, step = 0.5)
}

cost_server <- function(id, r_store) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$cost, {
      r_store[[id]] <- as.numeric(input$cost)
    }, ignoreNULL = TRUE)

  })
}
