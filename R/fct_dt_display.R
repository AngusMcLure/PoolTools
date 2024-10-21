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
  df %>%
    rename(
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
  df %>%
    rename(
      `Number of Pools` = NumberOfPools,
      `Number of Positive Pools` = NumberPositive
    )
}

#' @rdname rename_cols
rename_ICC <- function(df) {
  col_names <- names(df)
  new_col_names <- col_names
  new_col_names[grep("ICC", col_names)] <-
    new_col_names[grep("ICC", col_names)] %>%
    {
      gsub("ICC_CrILow", "Lower Credible Interval (95%)", .)
    } %>%
    {
      gsub("ICC_CrIHigh", "Upper Credible Interval (95%)", .)
    }
  names(df) <- new_col_names
  return(df)
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
#'
#' @name round_cols
round_with_trailing <- function(x, digits) {
  sprintf(paste0("%.", digits, "f"), round(x, digits))
}

#' @rdname round_cols
round_pool_cols <- function(df, digits, cols) {
  dplyr::mutate_at(df, cols, ~ round_with_trailing(., digits))
}

#' @rdname round_cols
signif_with_trailing <- function(x, digits) {
  sprintf(paste0("%.", (digits - 1), "e"), signif(x, digits))
}

#' @rdname round_cols
signif_pool_cols <- function(df, digits, cols) {
  dplyr::mutate_at(df, cols, ~ signif_with_trailing(., digits))
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

#' Create Labels for Prevalence Output Columns
#'
#' The development of intervention plans are often determined based on the
#' prevalence per a given value (e.g. multiplying by 2000 gives you the
#' prevalence per 2000 units). This function creates column labels to explicitly
#' state the prevalence per a given value in the output data frame
#'
#' @column_names character Column names taken from the PoolTestR output
#' @param ptr_mode character String indicating PoolTestR mode used. Get from
#' `which_pooltestr`.
#' @param per_val integer Value to multiply prevalence and intervals by.
#'
#' @return Returns a label, which is appended to Prevalence (both MLE and
#' Bayesian) column names
#'
#' @keywords internal
per_unit_label <- function(column_names, ptr_mode, per_val) {
  if (per_val != 1){
    new_column_names <- column_names
    if (per_val == 100){
      # Percentage (%)
      label_suffix <- paste0("%")
    } else {
      label_suffix <- paste0("per ", per_val, " units")
    }
    if (ptr_mode %in% c("poolprev", "poolprev_strat")) {
      labs_to_update <- which(
        new_column_names == "Prevalence (Maximum Likelihood Estimate)"
      )
    } else if (ptr_mode %in% c("poolprev_bayes", "poolprev_bayes_strat")) {
      labs_to_update <- which(
        new_column_names == "Prevalence (Maximum Likelihood Estimate)" |
          new_column_names == "Prevalence (Bayesian)"
      )
    } else if (ptr_mode %in% c("hierpoolprev", "hierpoolprev_strat")){
      labs_to_update <- which(new_column_names == "Prevalence (Bayesian)")
    }
    new_column_names[labs_to_update] <-
      unlist(
        lapply(
          labs_to_update,
          function(x){
            new_column_names[x] <- paste(new_column_names[x],
                                         label_suffix, sep = " ")
          }
        )
      )
    return(new_column_names)
  } else {
    return(column_names)
  }
}

#' Display Prevalence and CI/CrI Per Value
#'
#' The development of intervention plans are often determined based on the
#' prevalence per a given value (e.g. multiplying by 2000 gives you the
#' prevalence per 2000 units). This function applies this transformation across
#' columns, as well as rounding (see note on rounding above).
#'
#' TODO handle negative input for per_val
#'
#' @param df dataframe PoolTestR output
#' @param ptr_mode character String indicating PoolTestR mode used. Get from
#' `which_pooltestr`.
#' @param per_val integer Value to multiply prevalence and intervals by. When
#' `per_prev` is TRUE, `per_val` = 1 (i.e., no multiplication)
#' @param digits integer Number of digits to round by.
#' @param per_prev boolean Should values be displayed `per_val`? `TRUE` by
#' default.
#'
#' @return dataframe
#' @name dt_display
dt_display <- function(df, ptr_mode, per_val, digits, per_prev = TRUE) {
  # Check whether prevalence is to multiplied by per_val
  if (per_prev == FALSE){
    # Reset per_val to 1
    per_val <- 1
  }
  # poolprev_bayes requires both transformations
  if (ptr_mode %in% c("poolprev", "poolprev_strat", "poolprev_bayes", "poolprev_bayes_strat")) {
    df <- df %>%
      multiply_cols(mle_cols, per_val) %>%
      round_pool_cols(digits = digits, cols = mle_cols)
  }
  if (ptr_mode %in% c("poolprev_bayes", "poolprev_bayes_strat", "hierpoolprev", "hierpoolprev_strat")) {
    df <- df %>%
      multiply_cols(bayes_cols, per_val) %>%
      round_pool_cols(digits = digits, cols = bayes_cols)
  }
  names(df) <- per_unit_label(
    column_names = names(df), ptr_mode = ptr_mode, per_val = per_val
  )
  # extract all columns from ICC using clustering variables
  clustering_vars <- gsub(" ICC", "", grep("ICC", names(df), value = TRUE))
  icc_col_inds <- grep(paste(clustering_vars, collapse = "|"), names(df))
  icc_col_names <- names(df)[icc_col_inds]
  if (length(icc_col_names) > 0) {
    min_col_values <- unlist(lapply(icc_col_inds, function(i) {
      min(df[, i])
    }))
    check_val <- 1*10^(-digits) # display ICC output with `digits` sig. figs.
    icc_cols_round <- icc_col_names[which(min_col_values >= check_val)]
    icc_cols_sf <- icc_col_names[which(min_col_values < check_val)]
    df <- df %>%
      round_pool_cols(digits = digits, cols = icc_cols_round) %>%
      signif_pool_cols(digits = digits, cols = icc_cols_sf)
  }
  return(df)
}

#' @rdname dt_display
multiply_cols <- function(df, cols, val) {
  dplyr::mutate_at(df, cols, ~ as.numeric(.) * val)
}

#' Display ICCs
#'
#' Helper for handling ICC matrix columns in `PoolTestR::HierPoolPrev` output.
#' If ICC columns are not present, this function returns the input. If ICC
#' columns are present, this function separates the ICC columns for each
#' category into a separate matrix.
#' See `run_pooltestr()` for details on the different PoolTestR modes.
#'
#' @param df dataframe Output of PoolTestR::HierPoolPrev()
#'
#' @return dataframe
#'
#' @keywords internal
reformat_ICC_cols <- function(df) {
  # Remove ICC columns
  icc_names <- attr(df$ICC, "dimnames")[[2]]
  trimmed_object <- df %>%
    select(-contains("ICC", ignore.case = TRUE))
  # Reformat matrix columns by clustering variables and reattach to df
  icc_tbls <- lapply(icc_names, extract_matrix_column_ICC, df)
  icc_output <- as.data.frame(bind_cols(trimmed_object, icc_tbls))
  return(icc_output)
}

#' @keywords internal
extract_matrix_column_ICC <- function(cluster_var, df) {
  all_cluster_vars <- attr(df$ICC, "dimnames")[[2]]
  if (cluster_var %in% all_cluster_vars) {
    # Extract only the columns for this clustering variable
    matrix_cols <- df %>%
      select(grep("ICC", names(df), value = T))
    cluster_cols <- as_tibble(
      lapply(
        names(matrix_cols),
        function(x) {
          matrix_cols[[x]][, which(all_cluster_vars == cluster_var)]
        }
      ),
      .name_repair = "minimal"
    )
    names(cluster_cols) <- paste0(cluster_var, " ", names(matrix_cols))
    return(cluster_cols)
  } else {
    return(NULL)
  }
}
