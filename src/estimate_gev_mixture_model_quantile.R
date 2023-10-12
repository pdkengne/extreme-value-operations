source("./src/estimate_gev_parameters.R")
source("./src/estimate_gev_model_parameters.R")
source("./src/shift_data_elements_circularly.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/estimate_gev_model_quantile.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")

estimate_gev_mixture_model_quantile <- function(gev_mixture_model, 
                                                alpha = NULL, 
                                                confidence_level = 0.95, 
                                                do.ci = TRUE,
                                                estimator_type = c("automatic_weights_mw", 
                                                                   "pessimistic_weights_mw", 
                                                                   "identic_weights_mw", 
                                                                   "automatic_weights_pw",
                                                                   "pessimistic_weights_pw", 
                                                                   "identic_weights_pw", 
                                                                   "empirical",
                                                                   "confidence_interval_mw",
                                                                   "confidence_interval_pw")[1]){
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_mixture_model_parameters()"
  # alpha: order of the quantile to estimate
  # confidence_level: the desired confidence level for the estimated quantile
  # do.ci: boolean which indicates whether to return confidence interval or not
  # estimator_type: quantile estimator to use from the set 
  # c("automatic_weights_mw", "pessimistic_weights_mw", "identic_weights_mw", "automatic_weights_pw","pessimistic_weights_pw", 
  #   "identic_weights_pw", "empirical", "confidence_interval_mw", "confidence_interval_pw")
  
  # extract the raw data
  raw_data <- gev_mixture_model$data
  
  # extract the largest train data
  data_largest <- gev_mixture_model$data_largest
  
  # calculate the proportion of largest data
  tau <- length(data_largest)/length(raw_data)
  
  # set the appropriate quantile order
  alpha_prime <- alpha/tau
  
  # set the types of weighted gev models
  weighted_gev_model_types = c("identic_weights", "pessimistic_weights", "automatic_weights")
  
  # extract the model parameters (pw)
  gev_model_parameters <- gev_mixture_model$weighted_normalized_gev_parameters_object
  
  # extract the model parameters (mw)
  gev_mixture_model_parameters <- gev_mixture_model$normalized_gev_parameters_object
  gev_mixture_model_parameters_object <- data.frame(cbind(gev_mixture_model_parameters$loc_star,
                                                          gev_mixture_model_parameters$scale_star,
                                                          gev_mixture_model_parameters$shape_star))
  names(gev_mixture_model_parameters_object) <- c("loc_star", "scale_star", "shape_star")
  
  # extract the weight parameters (mw)
  gev_mixture_model_weights_object <- data.frame(cbind(gev_mixture_model$identic_weights_mw,
                                                       gev_mixture_model$pessimistic_weights_mw,
                                                       gev_mixture_model$automatic_weights_mw))
  names(gev_mixture_model_weights_object) <- weighted_gev_model_types
  
  # extract the list of all estimated gev models
  gev_models_object <- gev_mixture_model$gev_models_object
  
  # extract the vector of block sizes
  block_sizes <- gev_mixture_model$block_sizes
  
  if (estimator_type == "identic_weights_pw"){
    output <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)

    output[1, 2] <- calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                              loc = gev_model_parameters["identic_weights", "loc_star"],
                                              scale = gev_model_parameters["identic_weights", "scale_star"], 
                                              shape = gev_model_parameters["identic_weights", "shape_star"])
  
    output
  }
  else if (estimator_type == "pessimistic_weights_pw"){
    output <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
    
    output[1, 2] <- calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                              loc = gev_model_parameters["pessimistic_weights", "loc_star"],
                                              scale = gev_model_parameters["pessimistic_weights", "scale_star"], 
                                              shape = gev_model_parameters["pessimistic_weights", "shape_star"])
    
    output
  }
  else if (estimator_type == "automatic_weights_pw"){
    output <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
    
    output[1, 2] <- calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                              loc = gev_model_parameters["automatic_weights", "loc_star"],
                                              scale = gev_model_parameters["automatic_weights", "scale_star"], 
                                              shape = gev_model_parameters["automatic_weights", "shape_star"])
    
    output
  }
  else if (estimator_type == "identic_weights_mw"){
    output <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
    
    output[1, 2] <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                            locations = gev_mixture_model_parameters_object$loc_star, 
                                                            scales = gev_mixture_model_parameters_object$scale_star, 
                                                            shapes = gev_mixture_model_parameters_object$shape_star, 
                                                            weights = gev_mixture_model_weights_object[, "identic_weights"],
                                                            iterations = 100)
    
    output
  }
  else if (estimator_type == "pessimistic_weights_mw"){
    output <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
    
    output[1, 2] <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                            locations = gev_mixture_model_parameters_object$loc_star, 
                                                            scales = gev_mixture_model_parameters_object$scale_star, 
                                                            shapes = gev_mixture_model_parameters_object$shape_star, 
                                                            weights = gev_mixture_model_weights_object[, "pessimistic_weights"],
                                                            iterations = 100)
    
    output
  }
  else if (estimator_type == "automatic_weights_mw"){
    output <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)

    output[1, 2] <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                            locations = gev_mixture_model_parameters_object$loc_star, 
                                                            scales = gev_mixture_model_parameters_object$scale_star, 
                                                            shapes = gev_mixture_model_parameters_object$shape_star, 
                                                            weights = gev_mixture_model_weights_object[, "automatic_weights"],
                                                            iterations = 100)
    
    output
  }
  else if (estimator_type == "empirical"){
    output <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
    
    quantile_alpha <- quantile(x = raw_data, probs = 1 - alpha)
    
    output[1, 2] <- quantile_alpha
    
    output
  }
  else if (estimator_type == "confidence_interval_mw"){
    # extract the vector of weights
    weights = gev_mixture_model_weights_object[, "automatic_weights"]
    
    # get the positions where weights are different from zero
    position_weights_nonzero <- which(weights > 0)
    
    # extract all parameters for which weights are different from zero
    block_sizes <- block_sizes[position_weights_nonzero]
    gev_models_object <- gev_models_object[position_weights_nonzero]
    
    if (max(block_sizes)*alpha_prime < 1){
      quantiles_object <- sapply(1:length(block_sizes), function(j){
        block_size <- block_sizes[j]
        
        gev_model <- gev_models_object[[j]]
        
        maxima <- gev_model$x
        
        out <- estimate_gev_model_quantile(x = maxima, alpha = alpha_prime*block_size)
        
        out})
      
      output <- data.frame(t(quantiles_object))
      
      rownames(output) <- block_sizes
      
      output
    }
    else{
      print("Please, enter a smaller quantile order")
    }
    
  }
  else{
    if (max(block_sizes)*alpha_prime < 1){
      quantiles_object <- sapply(1:length(block_sizes), function(j){
        block_size <- block_sizes[j]
        
        gev_model <- gev_models_object[[j]]
        
        maxima <- gev_model$x
        
        out <- estimate_gev_model_quantile(x = maxima, alpha = alpha_prime*block_size)
        
        out})
      
      output <- data.frame(t(quantiles_object))
      
      rownames(output) <- block_sizes
      
      output
    }
    else{
      print("Please, enter a smaller quantile order")
    }
    
  }
  
}


# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/calculate_gev_inverse_cdf.R")
# source("./src/estimate_gev_mixture_model_parameters.R")
# 
# n <- 100000
# nlargest <- 1000
# 
# x <- rnorm(n = n)
# 
# gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
#                                                            block_sizes = NULL,
#                                                            minimum_nblocks = 50,
#                                                            nlargest = nlargest,
#                                                            confidence_level = 0.95,
#                                                            trace = TRUE)
# 
# gev_mixture_model$normalized_gev_parameters_object
# 
# gev_mixture_model$weighted_normalized_gev_parameters_object
# 
# gev_mixture_model$automatic_weights_mw_statistics
# 
# gev_mixture_model$automatic_weights_pw_statistics
# 
# gev_mixture_model$automatic_weights_mw
# 
# 
# estimator_types <- c("automatic_weights_mw",
#                     "pessimistic_weights_mw",
#                     "identic_weights_mw",
#                     "automatic_weights_pw",
#                     "pessimistic_weights_pw",
#                     "identic_weights_pw",
#                     "empirical",
#                     "confidence_interval_mw",
#                     "confidence_interval_pw")
# 
# 
# alpha <- 10^(-14)
# 
# results_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                alpha = alpha,
#                                                confidence_level = 0.95,
#                                                do.ci = TRUE,
#                                                estimator_type = estimator_types[1])
# 
# results_mw
# 
# results_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                   alpha = alpha,
#                                                   confidence_level = 0.95,
#                                                   do.ci = TRUE,
#                                                   estimator_type = estimator_types[4])
# 
# results_pw
# 
# quantile(x = x, probs = 1 - alpha)
# 
# true_rl <- qnorm(p = 1 - alpha)
# true_rl
# 
# est_rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                  alpha = alpha,
#                                                  confidence_level = 0.95,
#                                                  do.ci = TRUE,
#                                                  estimator_type = estimator_types[9])
# 
# est_rl_pw
# 
# est_rl_pw_range <- range(as.matrix(est_rl_pw))
# est_rl_pw_range
# 
# 
# est_rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                  alpha = alpha,
#                                                  confidence_level = 0.95,
#                                                  do.ci = TRUE,
#                                                  estimator_type = estimator_types[8])
# 
# est_rl_mw
# 
# est_rl_mw_range <- range(as.matrix(est_rl_mw))
# est_rl_mw_range
# 
# 
# matplot(rownames(est_rl_pw), est_rl_pw, type = "l", lty = c("dotted", "solid", "dotted"), lwd = 2, col = c(3, 1, 3))
# 
# abline(h = true_rl, col = 4, lwd = 2)
# abline(h = results_mw[2], col = 7, lwd = 2)
# abline(h = results_pw[2], col = 6, lwd = 2)
# abline(h = est_rl_pw_range, col = 6, lty = "dotted", lwd = 2)
# abline(h = est_rl_mw_range, col = 7, lty = "dotted", lwd = 2)
# 
# 
# source("./src/plot_gev_mixture_model_pdf.R")
# 
# 
# plot_gev_mixture_model_pdf(gev_mixture_model,
#                            type = "automatic_weights",
#                            model_wise = FALSE,
#                            zoom = FALSE,
#                            xlab = "Quantile",
#                            ylab = "Density",
#                            main = "Probability Density Function (PDF) Plot")
# 
# plot_gev_mixture_model_pdf(gev_mixture_model,
#                            type = "automatic_weights",
#                            model_wise = FALSE,
#                            zoom = TRUE,
#                            xlab = "Quantile",
#                            ylab = "Density",
#                            main = "Probability Density Function (PDF) Plot")
# 
# 
# plot_gev_mixture_model_pdf(gev_mixture_model,
#                            type = "automatic_weights",
#                            model_wise = TRUE,
#                            zoom = FALSE,
#                            xlab = "Quantile",
#                            ylab = "Density",
#                            main = "Probability Density Function (PDF) Plot")
# 
# plot_gev_mixture_model_pdf(gev_mixture_model,
#                            type = "automatic_weights",
#                            model_wise = TRUE,
#                            zoom = TRUE,
#                            xlab = "Quantile",
#                            ylab = "Density",
#                            main = "Probability Density Function (PDF) Plot")
