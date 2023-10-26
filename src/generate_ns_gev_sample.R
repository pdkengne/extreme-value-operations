# library(extRemes)

generate_ns_gev_sample <- function(ns_gev_model, n = 1){
  # ns_gev_model: an object associated with a result of the function "estimate_ns_gev_parameters()"
  # n: vnumber of observations to generate
  
  # generate a sample from a fitted (non-stationary) extreme value distribution
  gev_sample <- extRemes::rextRemes(x = ns_gev_model, n = n)
  
  gev_sample
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
# result <- generate_ns_gev_sample(ns_gev_model = ns_gev_model, n = 10)
# 
# result
# 
# 
# # example 2
# 
# source("./src/estimate_ns_gev_parameters.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 100
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
# result <- generate_ns_gev_sample(ns_gev_model = ns_gev_model, n = 5)
# 
# result
# 
# 
# # example 3
# 
# source("./src/estimate_ns_gev_parameters.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 100
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
# result <- generate_ns_gev_sample(ns_gev_model = ns_gev_model, n = 5)
# 
# result
