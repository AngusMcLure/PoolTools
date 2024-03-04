# Helper functions for validation ----
is_filled <- function(input) {
  # Checks for non-empty reactive input
  !is.null(input) && input != ""
}

need_gt0 <- function(expr, var) {
  # amended shiny::need()
  message <- paste("Error:", var, "must be > 0")
  force(message)
  if (!isTruthy(expr > 0)) {
    return(message)
  } else {
    return(invisible(NULL))
  }
}

need_ge0 <- function(expr, var) {
  # amended shiny::need()
  message <- paste("Error:", var, "must be >= 0")
  force(message)
  if (!isTruthy(expr >= 0)) {
    return(message)
  } else {
    return(invisible(NULL))
  }
}

in_range <- function(input = input, inputId, range, inc_lower) {
  otherId <- paste0(inputId, "Other")
  if (input[[inputId]] == "other") {
    if (inc_lower) {
      return(input[[otherId]] >= range[1] && input[[otherId]] <= range[2])
    } else {
      return(input[[otherId]] > range[1] && input[[otherId]] <= range[2])
    }
  }
  TRUE # Return TRUE by default if condition is not "other"
}

## fileAnalyse column checks
is_binary_col <- function(df, col) {
  all(unique(df[[col]]) == c(0, 1))
}

is_integer_col <- function(df, col) {
  is.numeric(df[[col]]) && all(floor(df[[col]]) == df[[col]])
}
