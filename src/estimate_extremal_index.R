# library(evd)

estimate_extremal_index <- function(x, threshold, run = 0){
  # x: vector of observations
  # threshold: threshold above which (non-inclusive) the extremal index should be calculated
  # run: a positive integer denoting the clustering interval length (the number of threshold deficits to
  #      be considered as starting a new cluster). If run = 0, the intervals estimator of Ferro and Segers (2003) is used.
  
  extremal_index <- evd::exi(x, u = threshold, r = run)
  
  extremal_index
}


# # example 1
# 
# source("./src/find_minimum_threshold.R")
# 
# x <- rexp(n = 1000)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# result <- estimate_extremal_index(x, threshold, run = 0)
# result
# 
# 
# # example 2
# 
# source("./src/find_minimum_threshold.R")
# 
# x <- EnvStats::rzmnorm(n = 1000, mean = 0, sd = 1, p.zero = 0.5)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# result <- estimate_extremal_index(x, threshold, run = 0)
# result
# 
# 
# # example 3
# 
# source("./src/find_minimum_threshold.R")
# 
# # A max autoregressive process
# x <- evd::mar(1000, psi = 0.85)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# result <- estimate_extremal_index(x, threshold, run = 0)
# result
# 
# 
# # example 4
# 
# source("./src/find_minimum_threshold.R")
# 
# # A max autoregressive moving average process
# x <- evd::marma(100, p = 1, q = 1, psi = 0.75, theta = 0.65)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# result <- estimate_extremal_index(x, threshold, run = 0)
# result

