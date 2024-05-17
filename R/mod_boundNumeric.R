#' Input and storage of `numericInput()`.
#'
#' This ui/server pair ensures that inputs are stored in a separate reactive
#' `r_store` that objects in the input namespace are 'bound' to. The `r_store`
#' MUST be initialised outside of the modules. This enables input values to have
#' a consistent state in a dynamic UI (i.e. values do not reset when UI is
#' hidden or removed).
#'
#' `syncNumericInput()` updates the relevant object in `r_store` whenever a
#' `boundNumericInput()` receives a new value.
#'
#' @param id
#' @param r_store `reactiveValues()` object that stores costs.
boundNumericInput <- function(id, label, r_store) {
  ns <- NS(id)
  numericInput(
    ns("value"),
    label,
    # Ensures that the r_store and UI don't get updated infinitely
    value = isolate(r_store[[id]]),
    min = 1e-6,
    step = 0.5
  )
}

syncNumericInput <- function(id, r_store) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$value, {
      r_store[[id]] <- as.numeric(input$value)
    }, ignoreNULL = TRUE)
  })
}
