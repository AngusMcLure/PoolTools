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
      if (stratify) {
        # 4. PoolPrev with Bayesian calculation - Stratified
        return("poolprev_bayes_strat")
      } else {
        # 3. PoolPrev with Bayesian calculation - Unstratified
        return("poolprev_bayes")
      }
    } else if (stratify) {
      return("poolprev_strat") # 2. PoolPrev - Stratified
    } else {
      return("poolprev") # 1. PoolPrev - Whole (Unstratified)
    }
  }
  # safety check
  return(NULL)
}


#' Run PoolTestR
#'
#' `pooltestr_mode` is output from \code{which_pooltestr}, and determines
#' the calculations performed by `PoolTestR`
#'
#' @param pooltestr_mode
#' @param req_args
#' @param hier_vars
#' @param stratify_vars
#'
#' @return dataframe
#' @name run_pooltestr
#' @seealso \code{\link{which_pooltestr}}

run_pooltestr <- function(pooltestr_mode, req_args, hier_vars, stratify_vars) {
  if (pooltestr_mode == "poolprev" | pooltestr_mode == "poolprev_strat" |
      pooltestr_mode == "poolprev_bayes" | pooltestr_mode == "poolprev_bayes_strat") {
    # Add Bayesian switch for all PoolPrev modes
    poolprev_args <- req_args
    if (pooltestr_mode == "poolprev" | pooltestr_mode == "poolprev_strat"){
      poolprev_args$bayesian <- FALSE
    } else if (pooltestr_mode == "poolprev_bayes" | pooltestr_mode == "poolprev_bayes_strat"){
      poolprev_args$bayesian <- TRUE
    }
    # Run PoolTestR and format output
    if (pooltestr_mode == "poolprev" | pooltestr_mode == "poolprev_bayes") {
      # Run PoolTestR (estimate prevalence on whole data) for:
      #     1. PoolPrev
      #     3. PoolPrev (Bayesian)
      data <-
        do.call(PoolTestR::PoolPrev, poolprev_args) %>%
        rename_mle() %>%
        rename_pools()
    } else {
      # Run PoolTestR (estimate prevalence for each selected column) for:
      #     2. PoolPrev (Stratified)
      #     4. PoolPrev (Bayesian, Stratified)
      col_args <- c(poolprev_args, lapply(stratify_vars, as.name))
      data <-
        do.call(PoolTestR::PoolPrev, col_args) %>%
        rename_mle() %>%
        rename_pools()
    }
    # Format output for:
    #     3. PoolPrev (Bayesian)
    #     4. PoolPrev (Bayesian, Stratified)
    if (pooltestr_mode == "poolprev_bayes" | pooltestr_mode == "poolprev_bayes_strat") {
      data <- data %>%
        rename_bayes()
    }
  } else if (pooltestr_mode == "hierpoolprev" | pooltestr_mode == "hierpoolprev_strat") {
    # Account for hierarchical sampling structure
    hier_args <- req_args
    hier_args$hierarchy <- hier_vars
    if (pooltestr_mode == "hierpoolprev_strat") {
      # Prepare hierarchical input for:
      #   6. HierPoolPrev (Stratified)
      hier_args <- c(hier_args, lapply(stratify_vars, as.name))
    }
    # Run PoolTestR for:
    #     5. HierPoolPrev (Unstrat.)
    #     6. HierPoolPrev (Strat.)
    data <- do.call(PoolTestR::HierPoolPrev, hier_args)
    # Remove class
    # Format output
    if ("ICC" %in% names(data)){
      data <- data %>%
        reformat_ICC_cols() %>%
        rename_ICC()
    }
    data <- data %>%
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
