#' Determine PoolTestR Mode
#'
#' This logic simplifies how to identify which PoolTestR function was used based
#' on the UI inputs. This should be used when operating on specific columns based
#' on the PoolTestR mode is required. There are 6 possible modes with a unique
#' combination of the three parameters below.
#'
#' | ID | Input arguments          | bayesian  | stratify   | hierarchy |
#' | -- | ------------------------ | ----------| ---------- | --------- |
#' | 1  | PoolPrev                 | FALSE     | FALSE      | FALSE     |
#' | 2  | PoolPrev + strat         | FALSE     | TRUE       | FALSE     |
#' | 3  | PoolPrev + bayes         | TRUE      | FALSE      | FALSE     |
#' | 4  | PoolPrev + strat + bayes | TRUE      | TRUE       | FALSE     |
#' | 5  | HierPoolPrev             | NA (TRUE) | FALSE      | TRUE      |
#' | 6  | HierPoolPrev + strat     | NA (TRUE) | TRUE       | TRUE      |
#'
#' `bayesian` flag doesn't apply to HierPoolPrev as it is used by default.
#'
#' @param stratify
#' @param hierarchy
#' @param bayesian
#'
#' @return character String indicating mode used
which_pooltestr <- function(stratify, hierarchy, bayesian) {
  if (hierarchy) { # HierPoolPrev
    if (stratify) {
      # 6. Hierarchical and Stratified
      return("hierpoolprev_strat")
    } else {
      # 5. Hierarchical and Unstratified
      return("hierpoolprev")
    }
  } else { # PoolPrev
    if (bayesian) {
      # 3-4. PoolPrev with Bayesian calculation (applies to both stratified and non-stratified)
      return("poolprev_bayes")
    } else if (stratify) {
      return("poolprev_strat") # 2. Stratified
    } else {
      return("poolprev") # 1. Whole
    }
  }
  # safety check
  return(NULL)
}


#' Run PoolTestR
#'
#' TODO: Refactor so it uses ptr_mode as input
#'
#' @param req_args
#' @param stratify
#' @param hierarchy
#' @param hier_vars
#' @param bayesian
#' @param stratify_vars
#'
#' @return dataframe
#' @name run_pooltestr
run_pooltestr <- function(req_args, stratify, hierarchy, hier_vars, bayesian, stratify_vars) {
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

    # 3-4. PoolPrev (Bayesian) - either on whole or stratified
    if (bayesian) {
      data <- data %>%
        rename_bayes()
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
      rename_pools()
  } else {
    return(NULL)
  }

  if ("ProbAbsent" %in% names(data)) {
    data <- data %>% select(-ProbAbsent)
  }

  return(data)
}
