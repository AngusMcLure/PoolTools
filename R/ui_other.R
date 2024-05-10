processOther <- function(input, inputId) {
  otherId <- paste0(inputId, "Other")
  if (input[[inputId]] == "other") {
    as.numeric(input[[otherId]]) / 100
  } else {
    as.numeric(input[[inputId]])
  }
}
