#' Rename PoolTestR Columns
#'
#' Helpers for renaming columns from PoolTestR outputs. The different functions
#' are applied according to the PoolTestR mode run. Key differences are whether
#' MLE (prevalence + confidence intervals (CI)) or Bayesian (prevalence +
#' credible intervals (CrI)) are run and output. See `run_pooltestr()` for the
#' different modes.
#'
#' @param df dataframe Output of either PoolTestR::PoolPrev() or
#' PoolTestR::HierPoolPrev()
#'
#' @return dataframe
#' @name rename_cols
rename_mle <- function(df) {
  df %>% rename(
    `Prevalence (Maximum Likelihood Estimate)` = PrevMLE,
    `Lower Confidence Interval (95%)` = CILow,
    `Upper Confidence Interval (95%)` = CIHigh
  )
}

#' @rdname rename_cols
rename_bayes <- function(df) {
  df %>%
    rename(
      `Prevalence (Bayesian)` = PrevBayes,
      `Lower Credible Interval (95%)` = CrILow,
      `Upper Credible Interval (95%)` = CrIHigh
    )
}

#' @rdname rename_cols
rename_pools <- function(df) {
  df %>% rename(
    `Number of Pools` = NumberOfPools,
    `Number of Positive Pools` = NumberPositive
  )
}

#' Rounding PoolTestR Columns
#'
#' Allows users to specify how many digits to round to. Rounds Prevalence and
#' CI/CrI. Function and column names passed depends on whether MLE and/or
#' Bayesian analyses were used.
#'
#' Helper `round_with_trailing()` adds trailing 0s for nicer display. Currently,
#' rounding should be the last operation on the dataframe as it converts numerics
#' to characters before displaying as a datatable.
#'
#' @param x
#' @param digits integer Number of digits to round to
#' @param cols character vector Either mle or bayes column names to round
#' @name round_cols
round_with_trailing <- function(x, digits) {
  sprintf(paste0("%.", digits, "f"), round(x, digits))
}

#' @rdname round_cols
round_pool_cols <- function(df, digits, cols) {
  dplyr::mutate(df, across(cols, ~ round_with_trailing(., digits)))
}

#' @rdname round_cols
mle_cols <- c(
  "Prevalence (Maximum Likelihood Estimate)",
  "Lower Confidence Interval (95%)",
  "Upper Confidence Interval (95%)"
)

#' @rdname round_cols
bayes_cols <- c(
  "Prevalence (Bayesian)",
  "Lower Credible Interval (95%)",
  "Upper Credible Interval (95%)"
)

#' Display Prevalence and CI/CrI Per Value
#'
#' The development of intervention plans are often determined based on the
#' prevalence per a given value (e.g. 1/1000). This function applies this
#' transformation.
#'
#' @param df dataframe PoolTestR output
#' @param ptr_mode character String indicating PoolTestR mode used. Get from
#' `which_pooltestr`.
#' @param val integer Value to divide prevalence and intervals by.
#'
#' @return dataframe
#' @name prev_per_val
dt_display <- function(df, ptr_mode, divide_prev, divide_val, digits) {
  # poolprev_bayes requires both transformations
  if (!divide_prev) {
    # Don't divide
    val <- 1
  } else {
    val <- divide_val
  }
  if (ptr_mode %in% c("poolprev", "poolprev_strat", "poolprev_bayes")) {
    df <- df %>%
      divide_cols(mle_cols, val) %>%
      round_pool_cols(digits = digits, cols = mle_cols)
  }
  if (ptr_mode %in% c("poolprev_bayes", "hierpoolprev", "hierpoolprev_strat")) {
    df <- df %>%
      divide_cols(bayes_cols, val) %>%
      round_pool_cols(digits = digits, cols = bayes_cols)
  }
  return(df)
}

#' @rdname divide_cols
divide_cols <- function(df, cols, val) {
  dplyr::mutate(
    df,
    dplyr::across(cols, ~ as.numeric(.) / val)
  )
}
