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
#' CI/CrI. Function used depends on whether MLE and/or Bayesian analyses were
#' used.
#'
#' Helper `round_with_trailing()` adds trailing 0s for nicer display.
#'
#' @param df dataframe Output of either PoolTestR::PoolPrev() or
#' PoolTestR::HierPoolPrev()
#' @param round_digits integer Number of digits to round to
#' @param cols character Vector of column names to round. Pass either `mle_cols`
#' or `bayes_cols`
#'
#' @return df dataframe
#' @name round_cols
round_pool_cols <- function(df, round_digits, cols) {
  # cols either mle_cols or bayes_cols
  df %>% dplyr::mutate(
    across(cols, ~ round_with_trailing(., round_digits))
  )
}

#' @rdname round_cols
round_with_trailing <- function(x, digits) {
  sprintf(paste0("%.", digits, "f"), round(x, digits))
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

#' Run PoolTestR
#'
#' TODO: Refactor so it uses ptr_mode as input
#'
#' @param req_args
#' @param stratify
#' @param hierarchy
#' @param hier_vars
#' @param bayesian
#' @param round_digits
#' @param stratify_vars
#'
#' @return dataframe
#' @name run_pooltestr
run_pooltestr <- function(req_args, stratify, hierarchy, hier_vars, bayesian, round_digits, stratify_vars) {
  if (!hierarchy) {
    # Add bayesian switch for PoolPrev
    poolprev_args <- req_args
    poolprev_args$bayesian <- bayesian
    if (!stratify) {
      # 1. PoolPrev - Estimate prevalence on whole data
      data <-
        do.call(PoolTestR::PoolPrev, poolprev_args) %>%
        rename_mle() %>%
        rename_pools()
    } else {
      # 2. PoolPrev (Stratified): Estimate prevalence for each selected column
      col_args <- c(poolprev_args, lapply(stratify_vars, as.name))
      data <-
        do.call(PoolTestR::PoolPrev, col_args) %>%
        rename_mle() %>%
        rename_pools()
    }
    # Round
    data <- data %>% round_pool_cols(round_digits, mle_cols)

    # 3-4. PoolPrev (Bayesian) - either on whole or stratified
    if (bayesian) {
      data <- data %>%
        rename_bayes() %>%
        round_pool_cols(round_digits, bayes_cols)
    }
  } else if (hierarchy) {
    # Account for hierarchical sampling structure
    hier_args <- req_args
    hier_args$hierarchy <- hier_vars
    # 6. HierPoolPrev (Stratified)
    if (stratify) {
      hier_args <- c(hier_args, lapply(stratify_vars, as.name))
    }
    # 5. HierPoolPrev (Unstrat.)
    data <-
      do.call(PoolTestR::HierPoolPrev, hier_args) %>%
      rename_bayes() %>%
      rename_pools() %>%
      round_pool_cols(round_digits, bayes_cols)
  } else {
    return(NULL)
  }


  if ("ProbAbsent" %in% names(data)) {
    data <- data %>% select(-ProbAbsent)
  }

  return(data)
}
