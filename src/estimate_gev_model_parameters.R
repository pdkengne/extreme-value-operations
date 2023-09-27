# library(extRemes)

options(digits = 10)

estimate_gev_model_parameters <- function(x, 
                                          data = NULL, 
                                          threshold = NULL, 
                                          threshold.fun = ~1, 
                                          location.fun = ~1,
                                          scale.fun = ~1, 
                                          shape.fun = ~1, 
                                          use.phi = FALSE,
                                          type = c("GEV", "GP", "PP", "Gumbel", "Exponential")[1],
                                          method = c("MLE", "GMLE", "Bayesian", "Lmoments")[1]){
  # x: vector of observations (assumed to be block maxima)
  # data: dataframe of covariates for linear modeling of the location parameter
  # threshold.fun, location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                                    must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter or not
  # type: type of model to use
  # method: estimation method to use
  
  gev_model <- extRemes::fevd(x = x, 
                              data = data, 
                              threshold = threshold, 
                              threshold.fun = threshold.fun, 
                              location.fun = location.fun,
                              scale.fun = scale.fun, 
                              shape.fun = shape.fun, 
                              use.phi = use.phi,
                              type = type,
                              method = method)
  
  gev_model
}


# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# results <- estimate_gev_model_parameters(x)
# 
# results
# 
# results_summary <- summary(results, silent = TRUE)
# 
# results_summary
# 
# names(results_summary)
