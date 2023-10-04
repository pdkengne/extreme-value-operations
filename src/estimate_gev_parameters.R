# library(extRemes)

options(digits = 10)

estimate_gev_parameters <- function(x, 
                                    data = NULL, 
                                    location.fun = ~1,
                                    scale.fun = ~1, 
                                    shape.fun = ~1, 
                                    use.phi = FALSE,
                                    type = c("GEV", "Gumbel")[1],
                                    method = c("MLE", "GMLE", "Lmoments")[1]){
  # x: vector of observations (assumed to be block maxima)
  # data: dataframe of covariates for linear modeling of the location parameter
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                                    must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter or not
  # type: type of model to use
  # method: estimation method to use
  
  # check whether data is null
  if (is.null(data)){
    data <- data.frame("x" = x)
  }
  
  # estimation of the gev model
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
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)
# 
# results <- estimate_gev_parameters(x)
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
# results <- estimate_gev_parameters(x, method = c("MLE", "GMLE", "Lmoments")[3])
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
# results <- estimate_gev_parameters(x, method = c("MLE", "GMLE", "Lmoments")[2])
# 
# results
# names(results)
# 
# 
# # example 4
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 100, loc = 0.13, scale = 1.1, shape = 0.2)
# 
# trend <- (-49:50)/100
# rnd <- runif(100, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# results <- estimate_gev_parameters(x = x, 
#                                   data = data, 
#                                   location.fun = ~ .,
#                                   scale.fun = ~ ., 
#                                   shape.fun = ~ ., 
#                                   use.phi = TRUE,
#                                   method = c("MLE", "GMLE", "Lmoments")[1])
# 
# results
# names(results)
# summary <- summary(results, silent = TRUE)
# summary
# names(summary)
