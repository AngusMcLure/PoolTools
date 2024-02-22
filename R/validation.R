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
