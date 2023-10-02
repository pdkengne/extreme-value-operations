# library(evd)

source("./src/estimate_gev_model_parameters.R")

options(digits = 10)

estimate_gev_parameters <- function(x, ..., nsloc = NULL, prob = NULL, std.err = FALSE){
  # x: vector of observations (assumed to be block maxima)
  # nsloc: dataframe of covariates for linear modeling of the location parameter
  # prob: order of quantile associated with the parametrization (quantile, scale, shape)
  # std.err: a boolean which indicates whether the standard errors are returned or not
  
  initial_estimate <- estimate_gev_model_parameters(x = x)
  
  start_parameters <- initial_estimate$results$par
  
  start <- list(loc = start_parameters["location"],
                scale = start_parameters["scale"],
                shape = start_parameters["shape"])
  
  gev_model <- evd::fgev(x, 
                         start = start, 
                         ..., 
                         nsloc = nsloc, 
                         prob = prob, 
                         std.err = std.err,
                         corr = FALSE, 
                         method = "BFGS", 
                         warn.inf = TRUE)
  
  gev_model
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)
# 
# results<- estimate_gev_parameters(x, nsloc = NULL, std.err = FALSE)
# 
# results
# names(results)
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# results<- estimate_gev_parameters(x, nsloc = NULL, std.err = TRUE)
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
# results<- estimate_gev_parameters(x, nsloc = NULL)
# 
# results
# names(results)
# 
# 
# # example 4
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)
# 
# results<- estimate_gev_parameters(x, nsloc = NULL, prob = 0.02, scale = 0.5, shape = 0, std.err = TRUE)
# 
# results
# names(results)
# confint(object = results, level = 0.95)
