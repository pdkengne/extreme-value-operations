# library(extRemes)

options(digits = 10)

estimate_ns_gev_parameters <- function(x, 
                                       data = NULL, 
                                       location.fun = ~1,
                                       scale.fun = ~1, 
                                       shape.fun = ~1, 
                                       use.phi = FALSE,
                                       type = c("GEV", "Gumbel")[1],
                                       method = c("MLE", "GMLE")[1]){
  # x: vector of observations (assumed to be block maxima)
  # data: dataframe of covariates for linear modeling of the location parameter
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # type: type of model to use
  # method: estimation method to use
  
  # check if the provided dataset is null
  if (is.null(data)){
    data <- data.frame("intercept" = rep(1, length(x)))
  }
  
  # estimate the non-stationary gev model
  gev_model <- extRemes::fevd(x = x, 
                              data = data, 
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
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = +0.2)
# 
# 
# results <- estimate_ns_gev_parameters(x,
#                                       data = NULL,
#                                       location.fun = ~ times,
#                                       scale.fun = ~1,
#                                       shape.fun = ~1,
#                                       use.phi = TRUE,
#                                       type = c("GEV", "Gumbel")[1],
#                                       method = c("MLE", "GMLE")[1])
# 
# results
# names(results)
# 
# 
# # example 2
# 
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
# results <- estimate_ns_gev_parameters(x,
#                                       data = data,
#                                       location.fun = ~ .,
#                                       scale.fun = ~1,
#                                       shape.fun = ~1,
#                                       use.phi = TRUE,
#                                       type = c("GEV", "Gumbel")[1],
#                                       method = c("MLE", "GMLE")[1])
# 
# results
# names(results)
# 
# 
# # example 3
# 
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
# results <- estimate_ns_gev_parameters(x,
#                                       data = data,
#                                       location.fun = ~ .,
#                                       scale.fun = ~ .,
#                                       shape.fun = ~1,
#                                       use.phi = TRUE,
#                                       type = c("GEV", "Gumbel")[1],
#                                       method = c("MLE", "GMLE")[2])
# 
# results
# names(results)
# summary <- summary(results, silent = TRUE)
# summary
# names(summary)
