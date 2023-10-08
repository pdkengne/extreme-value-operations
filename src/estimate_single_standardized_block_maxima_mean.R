source("./src/estimate_single_gev_model.R")

estimate_single_standardized_block_maxima_mean <- function(x, 
                                                           block_size = 1, 
                                                           confidence_level = 0.95, 
                                                           method = c("MLE", "GMLE", "Lmoments")[1]){
  # x: vector of observations
  # block_size: size of blocks to consider
  # confidence_level: desired confidence level
  # method: estimation method to use
  
  # estimate gev model
  model <- estimate_single_gev_model(x = x, 
                                     block_size = block_size,
                                     method = method)
  
  # extract gev model parameters
  if (method != "Lmoments"){
    gev_model_parameters <- model$gev_model$results$par
  }
  else{
    gev_model_parameters <- model$gev_model$results
  }
  
  # extract block maxima
  block_maxima <- model$block_maxima
  
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
# results <- estimate_single_standardized_block_maxima_mean(x, block_size = 40, confidence_level = 0.95, method = c("MLE", "GMLE", "Lmoments")[1])
# 
# results
# 
# 
# # example 2
# 
# x <- rnorm(n = 1000)
# 
# results <- estimate_single_standardized_block_maxima_mean(x, block_size = 40, confidence_level = 0.95, method = c("MLE", "GMLE", "Lmoments")[2])
# 
# results
# 
# 
# # example 3
# 
# x <- rnorm(n = 1000)
# 
# results <- estimate_single_standardized_block_maxima_mean(x, block_size = 40, confidence_level = 0.95, method = c("MLE", "GMLE", "Lmoments")[3])
# 
# results

