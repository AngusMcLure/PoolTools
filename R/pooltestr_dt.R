# Helpers ----
rename_mle <- function(df) {
  df %>% rename(
    `Prevalence (Maximum Likelihood Estimate)` = PrevMLE,
    `Lower Confidence Interval (95%)` = CILow,
    `Upper Confidence Interval (95%)` = CIHigh
  )
}

rename_pools <- function(df) {
  df %>% rename(
    `Number of Pools` = NumberOfPools,
    `Number of Positive Pools` = NumberPositive,
  )
}

rename_bayes <- function(df) {
  df %>%
    rename(
      `Prevalence (Bayesian)` = PrevBayes,
      `Lower Credible Interval (95%)` = CrILow,
      `Upper Credible Interval (95%)` = CrIHigh
    )
}

round_with_trailing <- function(x, digits) {
  sprintf(paste0("%.", digits, "f"), round(x, digits))
}

round_mle <- function(df, round_digits) {
  df %>% dplyr::mutate(
    across(
      c(`Prevalence (Maximum Likelihood Estimate)`, `Lower Confidence Interval (95%)`, `Upper Confidence Interval (95%)`),
      ~ round_with_trailing(., round_digits)
    )
  )
}

round_bayes <- function(df, round_digits) {
  df %>% dplyr::mutate(
    across(
      c(`Prevalence (Bayesian)`, `Lower Credible Interval (95%)`, `Upper Credible Interval (95%)`),
      ~ round_with_trailing(., round_digits)
    )
  )
}
# PoolTestR ----

run_pooltestr <- function(req_args, stratify, hierarchy, hier_vars, bayesian, round_digits, stratify_vars) {
  if (!hierarchy) {
    # Add bayesian switch for PoolPrev
    poolprev_args <- req_args
    poolprev_args$bayesian <- bayesian
    if (!stratify) {
      # Estimate prevalence on whole data
      data <-
        do.call(PoolTestR::PoolPrev, poolprev_args) %>%
        rename_mle() %>%
        rename_pools()
    } else {
      # Estimate prevalence for each selected column (stratified)
      # Parse arguments
      col_args <- c(poolprev_args, lapply(stratify_vars, as.name))
      data <-
        do.call(PoolTestR::PoolPrev, col_args) %>%
        rename_mle() %>%
        rename_pools()
    }
    # Round
    data <- data %>% round_mle(round_digits)

    if (bayesian) {
      data <- data %>%
        rename_bayes() %>%
        round_bayes(round_digits)
    }


  } else if (hierarchy) {
    # Account for hierarchical sampling structure
    hier_args <- req_args
    hier_args$hierarchy <- hier_vars
    # Parse arguments for stratification
    if (stratify) {
      hier_args <- c(hier_args, lapply(stratify_vars, as.name))
    }
    data <-
      do.call(PoolTestR::HierPoolPrev, hier_args) %>%
      rename_bayes() %>%
      rename_pools() %>%
      round_bayes(round_digits)
  } else {
    return(NULL)
  }


  if ("ProbAbsent" %in% names(data)) {
    data <- data %>% select(-ProbAbsent)
  }

  return(data)
}
