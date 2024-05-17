#' Input and storage of "Design" Costs.
#'
#' Most importantly, `value = isolate(r_store[[id]])` ensures that previously
#' stored values are displayed when the UI changes. `isolate()` breaks the
#' circular reference to the `r_store`. `cost_server()` updates the relevant
#' `r_store` key whenever the `numericInput()` changes.
#'
#' @param id
#' @param r_store `reactiveValues()` object that stores costs.
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
