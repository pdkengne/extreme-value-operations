# library(evd)

options(digits = 10)

estimate_gp_parameters <- function(x, threshold){
  # x: vector of observations
  # threshold: threshold to consider
  
  gp_model <- evd::fpot(x, threshold, model = "gpd", npp = length(x), cmax = FALSE, 
                         r = 1, ulow = -Inf, rlow = 1, mper = NULL, std.err = TRUE, 
                         corr = FALSE, method = "BFGS", warn.inf = TRUE)
  
  gp_model
}



# # example 1
# 
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# results<- estimate_gp_parameters(x, threshold)
# 
# results
# names(results)
# 
# 
# # example 2
# 
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# results<- estimate_gp_parameters(x, threshold)
# 
# results
# names(results)
# 
# 
# # example 3
# 
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# results<- estimate_gp_parameters(x, threshold)
# 
# results
# names(results)
