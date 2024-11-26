#' Input and storage of `numericInput()`.
#'
#' This ui/server pair ensures that inputs are stored in a separate reactive
#' `r_store` that objects in the input namespace are 'bound' to. The `r_store`
#' MUST be initialised outside of the modules. This enables input values to have
#' a consistent state in a dynamic UI (i.e. values do not reset when UI is
#' hidden or removed).
#'
#' `saveNumericInput()` updates the relevant object in `r_store` whenever a
#' `boundNumericInput()` receives a new value.
#'
#' @param id
#' @param label chr UI label
#' @param r_store `reactiveValues()` object that stores the value.
boundNumericInput <- function(r_store, id, label, ...) {
  # Default values are for costs
  ns <- NS(id)
  numericInput(
    ns("value"),
    label,
    # Ensures that the r_store and UI don't get updated infinitely
    value = isolate(r_store[[id]]),
    ...
  )
}

saveNumericInput <- function(id, r_store) {
  # TODO: switch args positions to be consistent with UI
  moduleServer(id, function(input, output, session) {
    observeEvent(input$value,
      {
        r_store[[id]] <- as.numeric(input$value)
      },
      ignoreNULL = TRUE
    )
  })
}
