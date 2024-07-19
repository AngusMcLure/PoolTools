prev_sN <- c("optimise_sN_prevalence", "Identify cost-effective designs", "Estimate prevalence", "Fixed sample size")
prev_rand <- c("optimise_random_prevalence", "Identify cost-effective designs", "Estimate prevalence", "Fixed sampling period")
power <- c("power_pool", "Evaluate the power of existing designs (WIP)", "Estimate prevalence", "Fixed sample size")
power_rand <- c("pool_power_random", "Evaluate the power of existing designs (WIP)", "Estimate prevalence", "Fixed sampling period")
size <- c("sample_size_pool", "Calculate the required sample size (WIP)", "Estimate prevalence", "Fixed sample size")
size_rand <- c("sample_size_pool_random", "Calculate the required sample size (WIP)", "Estimate prevalence", "Fixed sampling period")
types <- data.frame(rbind(prev_sN, prev_rand, power, power_rand, size, size_rand))
names(types) <- c("func", "mode", "objective", "collection")

analysis_type <- function(types, mode, objective, collection) {
  types %>%
    filter(mode == mode & objective == objective & collection == collection) %>%
    pull(func)
}

mode = "Evaluate the power of existing designs (WIP)"
obj = "Estimate prevalence"
col = "Fixed sampling period"

analysis_type(types = types, mode = mode, objective = obj, collection = col)
