# library(extRemes)

calculate_ns_gev_cdf <- function(ns_gev_model, q, covariates = NULL, lower.tail = TRUE){
  # ns_gev_model: an object associated with a result of the function "estimate_ns_gev_parameters()"
  # q: vector of observations or quantiles
  # covariates: a named list whose names match the fitted model parameter names
  # lower.tail: a boolean which indicates whether to calculate probabilities are P[X <= x] (default) or P[X > x]
  
  # create a numeric matrix with rows the same length as q and columns equal to the number of parameters
  if (!is.null(covariates)){
    qcov <- extRemes::make.qcov(x = ns_gev_model, vals = covariates)
  }
  else{
    qcov <- NULL
  }
  
  # Calculate probabilities from a fitted (non-stationary) extreme value distribution
  gev_cdf <- extRemes::pextRemes(x = ns_gev_model, 
                                 q = q, 
                                 lower.tail = lower.tail,
                                 qcov = qcov)
  
  gev_cdf
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
# q <- quantile(x, probs = c(0.90, 0.95, 0.99))
# 
# result <- calculate_ns_gev_cdf(ns_gev_model = ns_gev_model,
#                                q = q, 
#                                covariates = NULL,
#                                lower.tail = TRUE)
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
# q <- quantile(x, probs = 0.95)
# q
# 
# covariates <- list(mu1 = 25/n, phi1 = 25/n, phi2 = runif(n = 1, min = -0.5, max = 0.5))
# 
# qcov <- extRemes::make.qcov(x = ns_gev_model, vals = covariates)
# qcov
# 
# result <- calculate_ns_gev_cdf(ns_gev_model = ns_gev_model,
#                                q = q, 
#                                covariates = covariates,
#                                lower.tail = TRUE)
# 
# result
# 
# 
# # example 3
# 
# source("./src/estimate_ns_gev_parameters.R")
# source("./src/generate_gev_sample.R")
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
#                                            scale.fun = ~ trend + random,
#                                            shape.fun = ~ .,
#                                            use.phi = FALSE,
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
# q <- quantile(x, probs = 0.95)
# q
# 
# covariates <- list(mu1 = 25/n, sigma1 = 25/n, sigma2 = runif(n = 1, min = -0.5, max = 0.5),
#                    xi1 = 25/n, xi2 = runif(n = 1, min = -0.5, max = 0.5))
# 
# qcov <- extRemes::make.qcov(x = ns_gev_model, vals = covariates)
# qcov
# 
# result <- calculate_ns_gev_cdf(ns_gev_model = ns_gev_model,
#                                q = q, 
#                                covariates = covariates,
#                                lower.tail = TRUE)
# 
# result
