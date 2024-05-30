#' selectInputOther UI module
#'
#' Module for UI components that has a main selectInput (with tooltips), and a
#' dynamic "other" option where users can input a numeric value.
#'
#' This module removes the need to define the conditional "other" numericInput
#' in the app definition/upstream modules.
#'
#' @param id
#' @param label
#' @param tooltip chr tooltip text decription.
#' @param choices chr vector for selectInput. One of the choices must be "other".
#' @param selected default selectInput value.
#' @param value should match the default for ns("main")
#' @param min
#' @param max
#' @param step
selectInputOther <- function(id, label, tooltip, choices, selected) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("select_ui")),
    uiOutput(ns("other_ui"))
  )
}

#' @rdname selectInputOther
selectInputOtherServer <- function(id, label, tooltip, choices, selected, min = 1e-6, max = 50, step = 0.01) {
  stopifnot("other" %in% choices)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Main selection
    output$select_ui <- renderUI({
      selectInputTT(
        ns("select"), label, tooltip = tooltip, choices = choices, selected = selected
      )
    })

    # Only show if "other" from main selection is selected
    output$other_ui <- renderUI({
      req(input$select == "other")
      numericInput(ns("other"), NULL, value = selected, min = min, max = max, step = step)
    })

    # return "other" value only if selected
    processed_value <- reactive({
      if (input$select == "other") {
        as.numeric(input$other / 100) # percentage to decimal
      } else {
        as.numeric(input$select)
      }
    })

    return(processed_value)
  })
}
