# library(extRemes)

source("./src/estimate_single_ns_gev_model.R")

estimate_single_ns_standardized_block_maxima_mean <- function(x, 
                                                              block_size = 1, 
                                                              confidence_level = 0.95, 
                                                              data = NULL, 
                                                              location.fun = ~1,
                                                              scale.fun = ~1, 
                                                              shape.fun = ~1, 
                                                              use.phi = TRUE,
                                                              type = c("GEV", "Gumbel")[1],
                                                              method = c("MLE", "GMLE")[1]){
  # x: vector of observations
  # block_size: size of blocks to consider
  # confidence_level: desired confidence level
  # data: dataframe of covariates for linear modeling of the gev model parameters
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # type: type of model to use
  # method: estimation method to use
  
  # estimate gev model
  model <- estimate_single_ns_gev_model(x = x, 
                                        block_size = block_size,
                                        data = data, 
                                        location.fun = location.fun,
                                        scale.fun = scale.fun, 
                                        shape.fun = shape.fun, 
                                        use.phi = use.phi,
                                        type = type,
                                        method = method)
  
  # calculate standardized block maxima
  standardized_block_maxima <- extRemes::trans(model$gev_model)
  
  # calculate the confidence interval of the standardized block maxima mean
  standardized_block_maxima_student_test <- t.test(x = standardized_block_maxima, 
                                                   alternative = "two.sided",
                                                   mu = 0, 
                                                   var.equal = FALSE,
                                                   conf.level = confidence_level)
  
  estimated_mean_confidence_interval <- rep(NA, 3)
  estimated_mean_confidence_interval[2] <- standardized_block_maxima_student_test$estimate
  estimated_mean_confidence_interval[c(1,3)] <- standardized_block_maxima_student_test$conf.int
  names(estimated_mean_confidence_interval) <- c("lower_bound", "estimate", "upper_bound")
  
  estimated_mean_confidence_interval
}


# # example 1
# 
# n <- 1000
# 
# x <- rnorm(n = n)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# results <- estimate_single_ns_standardized_block_maxima_mean(x = x,
#                                                              block_size = 40,
#                                                              confidence_level = 0.95,
#                                                              data = data,
#                                                              location.fun = ~ .,
#                                                              scale.fun = ~1,
#                                                              shape.fun = ~1,
#                                                              use.phi = TRUE,
#                                                              type = c("GEV", "Gumbel")[1],
#                                                              method = c("MLE", "GMLE")[1])
# 
# results
# 
# 
# # example 2
# 
# n <- 1000
# 
# x <- rnorm(n = n)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# results <- estimate_single_ns_standardized_block_maxima_mean(x = x,
#                                                              block_size = 40,
#                                                              confidence_level = 0.95,
#                                                              data = data,
#                                                              location.fun = ~ .,
#                                                              scale.fun = ~ .,
#                                                              shape.fun = ~1,
#                                                              use.phi = TRUE,
#                                                              type = c("GEV", "Gumbel")[1],
#                                                              method = c("MLE", "GMLE")[2])
# 
# results
# 
# 
# # example 3
# 
# n <- 1000
# 
# x <- rnorm(n = n)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# results <- estimate_single_ns_standardized_block_maxima_mean(x = x,
#                                                              block_size = 40,
#                                                              confidence_level = 0.95,
#                                                              data = data,
#                                                              location.fun = ~ .,
#                                                              scale.fun = ~ .,
#                                                              shape.fun = ~ .,
#                                                              use.phi = TRUE,
#                                                              type = c("GEV", "Gumbel")[1],
#                                                              method = c("MLE", "GMLE")[1])
# 
# results

