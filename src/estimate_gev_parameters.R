# library(evd)

options(digits = 10)

estimate_gev_parameters <- function(x, nsloc = NULL, std.err = FALSE){
  # x: vector of observations (assumed to be block maxima)
  # nsloc: dataframe of covariates for linear modeling of the location parameter
  # std.err: a boolean which indicates whether the standard errors are returned or not
  
  gev_model <- evd::fgev(x, nsloc = nsloc, prob = NULL, std.err = std.err,
                        corr = FALSE, method = "BFGS", warn.inf = TRUE)
  
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
