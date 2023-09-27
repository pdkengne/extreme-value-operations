source("./src/extract_block_maxima.R")
source("./src/estimate_gev_parameters.R")
source("./src/estimate_gev_model_parameters.R")


estimate_single_standardized_block_maxima_mean_nonstationary <- function(x, 
                                                                         block_size = 1, 
                                                                         confidence_level = 0.95,
                                                                         data = NULL, 
                                                                         threshold = NULL, 
                                                                         threshold.fun = ~1, 
                                                                         location.fun = ~1,
                                                                         scale.fun = ~1, 
                                                                         shape.fun = ~1, 
                                                                         use.phi = FALSE,
                                                                         type = c("GEV", "GP", "PP", "Gumbel", "Exponential")[1],
                                                                         method = c("MLE", "GMLE", "Bayesian", "Lmoments")[1]){
  # x: vector of observations
  # block_size: size of blocks to consider
  # confidence_level: desired confidence level
  # data: dataframe of covariates for linear modeling of the location parameter
  # threshold.fun, location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                                    must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter or not
  # type: type of model to use
  # method: estimation method to use
  
  # extract block maxima
  block_maxima <- extract_block_maxima(x, block_size)
  
  # estimate gev model
  gev_model <- estimate_gev_model_parameters(x = block_maxima, 
                                             data = data,
                                             threshold = threshold, 
                                             threshold.fun = threshold.fun, 
                                             location.fun = location.fun,
                                             scale.fun = scale.fun, 
                                             shape.fun = shape.fun, 
                                             use.phi = use.phi,
                                             type = type,
                                             method = method)
  
  # extract gev model parameters
  gev_model_parameters <- gev_model$results$par
  
  # calculate standardized block maxima
  standardized_block_maxima <- (block_maxima - gev_model_parameters["location"])/gev_model_parameters["scale"]
  
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
# x <- rnorm(n = 1000)
# 
# results <- estimate_single_standardized_block_maxima_mean_nonstationary(x, block_size = 25, confidence_level = 0.95)
# 
# results
# 
# 
# # example 2
# 
# x <- rnorm(n = 1000)
# 
# results <- estimate_single_standardized_block_maxima_mean_nonstationary(x, block_size = 40, confidence_level = 0.95)
# 
# results
