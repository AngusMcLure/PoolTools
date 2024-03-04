pluralise <- function(obj, text) {
  if (obj > 1) return(paste0(text, "s"))
  else text
}

sn_text <- function(snprev_output, clustered) {
  r <- snprev_output
  pre_txt <- "For the given inputs, the optimal design is to sample "
  p_units <- pluralise(r$s, "unit")

  if (clustered) {
    paste0(
      pre_txt, r$catch, " units per collection site, across ",
      r$N, " pools with ", r$s, " ", p_units, " each pool."
   )
  } else if (!clustered) {
    paste0(pre_txt, r$s, " ", p_units, " per pool.")
  }
}

strat_text <- function(randprev_output, pool_strat) {
  r <- randprev_output
  pre <- "For the given inputs, the optimal design is to distribute units "
  if (pool_strat == "pool_max_size") {
    paste0(
      pre, "in pools of size ", r$pool_strat_pars$max_size,
      " with any remainder placed in a single smaller pool."
    )
  } else if (pool_strat == "pool_target_number") {
    paste0(
      pre, "into ", r$pool_strat_pars$target_number,
      " equally sized pools, with no maximum pool size."
    )
  }
}

period_text <- function(randprev_output) {
  r <- randprev_output
  p_periods <- pluralise(r$periods, "collection period")
  paste0("Sampling should be conducted over ", r$periods, " ", p_periods, ".")
}

catch_text <- function(randprev_output, clustered) {
  r <- randprev_output
  p_units <- pluralise(r$catch$mean, "unit")
  if (clustered) c <- "per cluster "
  else c <- ""
  paste0(
    "We expect an average of ", r$catch$mean, " ", p_units, " (variance: ",
    r$catch$variance, ") ", "caught ", c, " per collection period."
  )
}

paste_randprev <- function() {
  if (!clustered && is.na(r$periods))
  design_result(
    tagList(p_strat, tags$br(), tags$br(), p_period, tags$br(), tags$br(), p_catch)
  )
}
