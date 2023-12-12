# library(extRemes)

options(digits = 10)

estimate_gp_parameters <- function(x,
                                   type = c("GEV", "Gumbel")[1],
                                   method = c("MLE", "GMLE", "Lmoments")[1]){
  # x: vector of observations (assumed to be excesses) 
  
  gp_model <- extRemes::fevd(x = x,
                             threshold = 0, 
                             type = type, 
                             method = method)
  
  gp_model
}



# # example 1
# 
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 1000, threshold = 0, scale = 0.5, shape = +0.2)
# 
# results<- estimate_gp_parameters(x)
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
# x <- generate_gp_sample(n = 1000, threshold = 0, scale = 0.5, shape = -0.2)
# 
# results<- estimate_gp_parameters(x)
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
# x <- generate_gp_sample(n = 1000, threshold = 0, scale = 0.5, shape = 0)
# 
# results<- estimate_gp_parameters(x)
# 
# results
# names(results)
