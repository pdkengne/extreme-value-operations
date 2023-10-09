# library(extRemes)

options(digits = 10)

estimate_ns_gev_return_level <- function(ns_gev_model, 
                                         return.period, 
                                         covariates = NULL, 
                                         do.ci = FALSE, 
                                         alpha = 0.05){
  # ns_gev_model: an object associated with a result of the function "estimate_ns_gev_parameters()"
  # return.period: vector of return periods
  # covariates: a named list whose names match the fitted model parameter names
  # do.ci: a boolean which indicates whether to estimate confidence interval for return levels in non-stationary gev models
  # alpha: the (1 - alpha) * 100 percent confidence level for confidence intervals of return levels in non-stationary gev models
  
  # create a numeric matrix with rows the same length as q and columns equal to the number of parameters
  if (!is.null(covariates)){
    qcov <- extRemes::make.qcov(x = ns_gev_model, vals = covariates)
  }
  else{
    qcov <- NULL
  }
  
  # Calculate return level from a fitted (non-stationary) extreme value distribution
  gev_inverse_cdf <- extRemes::return.level(x = ns_gev_model, 
                                            return.period = return.period, 
                                            qcov = qcov,
                                            do.ci = do.ci,
                                            alpha = alpha)
  
  gev_inverse_cdf
}



# # example 1
# 
# source("./src/estimate_ns_gev_parameters.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = +0.2)
# 
# ns_gev_model <- estimate_ns_gev_parameters(x = x,
#                                            data = NULL,
#                                            location.fun = ~1,
#                                            scale.fun = ~1,
#                                            shape.fun = ~1,
#                                            use.phi = TRUE,
#                                            type = c("GEV", "Gumbel")[1],
#                                            method = c("MLE", "GMLE")[1])
# 
# ns_gev_model
# 
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$par.models$term.names
# 
# ns_gev_model$results$par
# 
# return.period <- c(n, 10*n)
# 
# result <- estimate_ns_gev_return_level(ns_gev_model = ns_gev_model, 
#                                        return.period = return.period, 
#                                        covariates = NULL, 
#                                        do.ci = TRUE, 
#                                        alpha = 0.05)
# 
# result
# 
# 
# # example 2
# 
# source("./src/estimate_ns_gev_parameters.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# ns_gev_model <- estimate_ns_gev_parameters(x,
#                                            data = data,
#                                            location.fun = ~ trend,
#                                            scale.fun = ~ trend + random,
#                                            shape.fun = ~ 1,
#                                            use.phi = TRUE,
#                                            type = c("GEV", "Gumbel")[1],
#                                            method = c("MLE", "GMLE")[1])
# 
# ns_gev_model
# 
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$results$par
# 
# ns_gev_model$par.models$term.names
# 
# return.period <- c(n, 10*n)
# 
# covariates <- list(mu1 = 25/n, phi1 = 25/n, phi2 = runif(n = 1, min = -0.5, max = 0.5))
# 
# qcov <- extRemes::make.qcov(x = ns_gev_model, vals = covariates)
# qcov
# 
# result <- estimate_ns_gev_return_level(ns_gev_model = ns_gev_model, 
#                                        return.period = return.period, 
#                                        covariates = NULL, 
#                                        do.ci = FALSE, 
#                                        alpha = 0.05)
# 
# result
# 
# 
# result <- estimate_ns_gev_return_level(ns_gev_model = ns_gev_model, 
#                                        return.period = return.period, 
#                                        covariates = covariates, 
#                                        do.ci = FALSE, 
#                                        alpha = 0.05)
# 
# result
