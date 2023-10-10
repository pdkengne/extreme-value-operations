# library(extRemes)

options(digits = 10)

estimate_gev_parameters <- function(x, 
                                    type = c("GEV", "Gumbel")[1],
                                    method = c("MLE", "GMLE", "Lmoments")[1]){
  # x: vector of observations (assumed to be block maxima)
  # type: type of model to use
  # method: estimation method to use
  
  # estimation of the gev model
  gev_model <- extRemes::fevd(x = x, 
                              type = type,
                              method = method)
  
  gev_model
}



# example 1

source("./src/generate_gev_sample.R")

x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)

results <- estimate_gev_parameters(x)

results
names(results)

# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# results <- estimate_gev_parameters(x, method = c("MLE", "GMLE", "Lmoments")[3])
# 
# results
# names(results)
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)
# 
# results <- estimate_gev_parameters(x, 
#                                    type = c("GEV", "Gumbel")[2],
#                                    method = c("MLE", "GMLE", "Lmoments")[1])
# 
# results
# names(results)
