# library(extRemes)

source("./src/estimate_gev_model_parameters.R")

estimate_gev_model_quantile <- function(x, 
                                        data = NULL, 
                                        alpha = NULL,
                                        do.ci = TRUE,
                                        confidence_level = 0.95,
                                        qcov = NULL, 
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
  # alpha: order of the quantile to estimate
  # do.ci: boolean which indicates whether to return confidence interval or not
  # confidence_level: the desired confidence level for the estimated quantile
  # qcov: numeric matrix with rows the same length as q and columns equal to the number of parameters.
  #       This gives any covariate values for a non-stationary model
  # threshold.fun, location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                                    must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter or not
  # type: type of model to use
  # method: estimation method to use
  
  model <- estimate_gev_model_parameters(x = x, 
                                         data = data, 
                                         threshold = threshold, 
                                         threshold.fun = threshold.fun, 
                                         location.fun = location.fun,
                                         scale.fun = scale.fun, 
                                         shape.fun = shape.fun, 
                                         use.phi = use.phi,
                                         type = type,
                                         method = method)
  
  quantile <- extRemes::return.level(x = model, 
                                     return.period = 1/alpha, 
                                     alpha = 1 - confidence_level, 
                                     method = c("normal"), 
                                     do.ci = do.ci,
                                     qcov = qcov)
  
  output <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
  
  output[1, ] <- quantile[names(quantile)]
  
  output
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# results <- estimate_gev_model_quantile(x = x, 
#                                        alpha = 10^(-2),
#                                        do.ci = TRUE,
#                                        confidence_level = 0.95,
#                                        qcov = NULL, 
#                                        threshold = NULL, 
#                                        threshold.fun = ~1, 
#                                        location.fun = ~1,
#                                        scale.fun = ~1, 
#                                        shape.fun = ~1, 
#                                        use.phi = FALSE,
#                                        type = c("GEV", "GP", "PP", "Gumbel", "Exponential")[1],
#                                        method = c("MLE", "GMLE", "Bayesian", "Lmoments")[1])
# 
# results
