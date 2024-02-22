## Input UIs with tooltips ----
selectInputTT <- function(input_id, label, tooltip, choices, selected = NULL) {
  selectInput(
    input_id,
    tags$span(
      label,
      tipify(icon("info-circle"), tooltip, placement = "right")
    ),
    choices = choices,
    selected = selected
  )
}

textInputTT <- function(input_id, label, choices, tooltip, value = NULL, placeholder = NULL) {
  textInput(
    input_id,
    tags$span(
      label,
      tipify(icon("info-circle"), tooltip, placement = "right")
    ),
    value = value,
    placeholder = placeholder
  )
}

checkboxInputTT <- function(input_id, label, tooltip, value = TRUE) {
  checkboxInput(
    input_id,
    tags$span(
      tags$b(label),
      tipify(icon("info-circle"), tooltip, placement = "right")
    ),
    value = value
  )
}
