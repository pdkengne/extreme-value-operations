# library(extRemes)
# library(EnvStats)

predict_gev_parameters <- function(ns_gev_model, 
                                   covariates = NULL, 
                                   type = c("GEV", "Gumbel")[1],
                                   method = c("MLE", "GMLE", "Lmoments")[1]){
  # ns_gev_model: an object associated with a result of the function "estimate_ns_gev_parameters()"
  # covariates: a named list whose names match the fitted model parameter names
  # type: type of model to use
  # method: estimation method to use
  
  if (ns_gev_model$const.loc & ns_gev_model$const.scale & ns_gev_model$const.shape){
    gev_model <- ns_gev_model
  }
  else{
    if (!is.null(covariates)){
      # Create a numeric matrix with one rows and columns equal to the number of parameters.
      qcov <- extRemes::make.qcov(x = ns_gev_model, vals = covariates)
      
      # calculate empirical cdf object
      empirical_probability_object <- EnvStats::ecdfPlot(x = ns_gev_model$x, 
                                                         prob.method ="plot.pos", 
                                                         plot.pos.con = 0.375, 
                                                         plot.it = FALSE)
      
      # extract empirical cdf
      empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
      
      # extract empirical return period
      return.periods <- 1/(1 - empirical_cdf)
      
      # Calculate return level from a fitted (non-stationary) extreme value distribution
      return_levels <- extRemes::return.level(x = ns_gev_model, 
                                              return.period = return.periods, 
                                              qcov = qcov)
      
      # estimation of the gev model
      gev_model <- extRemes::fevd(x = as.numeric(return_levels), 
                                  type = type,
                                  method = method)
    }
    else{
      print("Please enter appropriate information in the argument: covariates!")
      gev_model <- NULL
    }
  }
  
  
  gev_model
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_ns_gev_parameters.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = +0.2)
# 
# ns_gev_model <- estimate_ns_gev_parameters(x,
#                                            data = NULL,
#                                            location.fun = ~1,
#                                            scale.fun = ~1,
#                                            shape.fun = ~1,
#                                            use.phi = TRUE,
#                                            type = c("GEV", "Gumbel")[1],
#                                            method = c("MLE", "GMLE")[1])
# 
# ns_gev_model
# names(ns_gev_model)
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$results$par
# 
# ns_gev_model$par.models$term.names
# 
# result <- predict_gev_parameters(ns_gev_model = ns_gev_model, 
#                                  covariates = NULL, 
#                                  type = c("GEV", "Gumbel")[1],
#                                  method = c("MLE", "GMLE", "Lmoments")[1])
# 
# result
# 
# names(result)
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_ns_gev_parameters.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# ns_gev_model <- estimate_ns_gev_parameters(x,
#                                            data = data,
#                                            location.fun = ~ trend,
#                                            scale.fun = ~ .,
#                                            shape.fun = ~ random,
#                                            use.phi = TRUE,
#                                            type = c("GEV", "Gumbel")[1],
#                                            method = c("MLE", "GMLE")[1])
# 
# ns_gev_model
# names(ns_gev_model)
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$results$par
# 
# ns_gev_model$par.models$term.names
# 
# names(data)
# 
# covariates <- list(mu1 = 25/n, 
#                    phi1 = 25/n, 
#                    phi2 = runif(n = 1, min = -0.5, max = 0.5),
#                    xi1 = runif(n = 1, min = -0.5, max = 0.5))
# 
# result <- predict_gev_parameters(ns_gev_model = ns_gev_model, 
#                                  covariates = covariates, 
#                                  type = c("GEV", "Gumbel")[1],
#                                  method = c("MLE", "GMLE", "Lmoments")[1])
# 
# result
# 
# names(result)
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_ns_gev_parameters.R")
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
#                                            scale.fun = ~ .,
#                                            shape.fun = ~1,
#                                            use.phi = TRUE,
#                                            type = c("GEV", "Gumbel")[1],
#                                            method = c("MLE", "GMLE")[2])
# 
# ns_gev_model
# names(ns_gev_model)
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$results$par
# 
# ns_gev_model$par.models$term.names
# 
# names(data)
# 
# covariates <- list(mu0 = 1, 
#                    mu1 = 25/n,
#                    phi0 = 1,
#                    phi1 = 25/n, 
#                    phi2 = runif(n = 1, min = -0.5, max = 0.5))
# 
# 
# result <- predict_gev_parameters(ns_gev_model = ns_gev_model, 
#                                  covariates = NULL, 
#                                  type = c("GEV", "Gumbel")[1],
#                                  method = c("MLE", "GMLE", "Lmoments")[1])
# 
# result
# 
# result <- predict_gev_parameters(ns_gev_model = ns_gev_model, 
#                                  covariates = covariates, 
#                                  type = c("GEV", "Gumbel")[1],
#                                  method = c("MLE", "GMLE", "Lmoments")[1])
# 
# result
# 
# names(result)
