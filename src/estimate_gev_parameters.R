# library(evd)

options(digits = 15)

estimate_gev_parameters <- function(x, nsloc = NULL){
  # x: vector of observations (assumed to be block maxima)
  # nsloc: dataframe of covariates for linear modeling of the location parameter
  
  gev_model <- evd::fgev(x, nsloc = nsloc, prob = NULL, std.err = FALSE,
                        corr = FALSE, method = "BFGS", warn.inf = TRUE)
  
  gev_model
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)
# 
# results<- estimate_gev_parameters(x, nsloc = NULL)
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
# results<- estimate_gev_parameters(x, nsloc = NULL)
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
