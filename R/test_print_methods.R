# Open package
library("PoolTestR")
# Generate output
PP_op <- PP_op <- PoolPrev(SimpleExampleData, Result, NumInPool, Region, Year, bayesian = FALSE)
HierPP_op <- HierPoolPrev(SimpleExampleData, Result, NumInPool, c("Village","Site"), Region, Year, cores = 6, verbose = F)
# Test print
print(PP_op)
print(HierPP_op)
