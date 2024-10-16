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
#' `pooltestr_mode` is the output from \code{which_pooltestr}
#'
#' @param req_args
#' @param stratify
#' @param hierarchy
#' @param hier_vars
#' @param bayesian
#' @param stratify_vars
#' @param pooltestr_mode
#'
#' @return dataframe
#' @name run_pooltestr
#' @seealso \code{\link{which_pooltestr}}
run_pooltestr <- function(req_args, stratify, hierarchy, hier_vars, bayesian, stratify_vars, pooltestr_mode) {

  if (!hierarchy | pooltestr_mode == "poolprev" | pooltestr_mode == "poolprev_strat" |
      pooltestr_mode == "poolprev_bayes" | pooltestr_mode == "poolprev_bayes_strat") {
    # Add Bayesian switch for PoolPrev
    poolprev_args <- req_args
    poolprev_args$bayesian <- bayesian
    if (!stratify | pooltestr_mode == "poolprev" | pooltestr_mode == "poolprev_bayes") {
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
    if (bayesian | pooltestr_mode == "poolprev_bayes" | pooltestr_mode == "poolprev_bayes_strat") {
      data <- data %>%
        rename_bayes()
    }
  } else if (hierarchy | pooltestr_mode == "hierpoolprev" | pooltestr_mode == "hierpoolprev_strat") {
    # Account for hierarchical sampling structure
    hier_args <- req_args
    hier_args$hierarchy <- hier_vars
    if (stratify | pooltestr_mode == "hierpoolprev_strat") {
      # Prepare hierarchical input for:
      #   6. HierPoolPrev (Stratified)
      hier_args <- c(hier_args, lapply(stratify_vars, as.name))
    }
    # Run PoolTestR (account for hierarchical sampling structure) for:
    #     5. HierPoolPrev (Unstrat.)
    #     6. HierPoolPrev (Strat.)
    data <-
      do.call(PoolTestR::HierPoolPrev, hier_args)

    ## Old formatting
    # data <-
    #   do.call(PoolTestR::HierPoolPrev, hier_args) %>%
    #   rename_bayes() %>%
    #   rename_pools()

    # If ICC columns are present, update formatting
    if ("ICC" %in% names(data)){
      data <- reformat_ICC_cols(data)
    }

  } else {
    return(NULL)
  }

  if ("ProbAbsent" %in% names(data)) {
    data <- data %>% select(-ProbAbsent)
  }

  return(data)
}
