source("./src/calculate_gev_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")

estimate_gev_mixture_model_quantile <- function(gev_mixture_model, 
                                                alpha = NULL, 
                                                estimator_type = c("automatic_weights_mw", 
                                                                   "pessimistic_weights_mw", 
                                                                   "identic_weights_mw", 
                                                                   "automatic_weights_pw",
                                                                   "pessimistic_weights_pw", 
                                                                   "identic_weights_pw", 
                                                                   "empirical",
                                                                   "gev_model_quantiles_mw",
                                                                   "gev_model_quantiles_pw")[1]){
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_mixture_model_parameters()"
  # alpha: order of the quantile to estimate
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
  
  # get the inclusive status of extremal index in gev models
  use_extremal_index <- gev_mixture_model$use_extremal_index
  
  # extract the model parameters (mw)
  if (use_extremal_index){
    gev_mixture_model_parameters_object <- gev_mixture_model$full_normalized_gev_parameters_object
  }
  else{
    gev_mixture_model_parameters_object <- gev_mixture_model$normalized_gev_parameters_object
  }
  
  # extract the weight parameters (mw)
  gev_mixture_model_weights_object <- data.frame(cbind(gev_mixture_model$identic_weights_mw,
                                                       gev_mixture_model$pessimistic_weights_mw,
                                                       gev_mixture_model$automatic_weights_mw))
  names(gev_mixture_model_weights_object) <- weighted_gev_model_types
  
  # extract the vector of block sizes
  block_sizes <- gev_mixture_model$block_sizes
  
  # quantile order for gev mixture model
  alpha_gev_mixture_model <- max(block_sizes)*alpha_prime
  
  if (alpha_gev_mixture_model < 1){
    if (estimator_type == "identic_weights_pw"){
      output <- calculate_gev_inverse_cdf(p = 1 - alpha_gev_mixture_model,
                                          loc = gev_model_parameters["identic_weights", "loc_star"],
                                          scale = gev_model_parameters["identic_weights", "scale_star"], 
                                          shape = gev_model_parameters["identic_weights", "shape_star"])
    }
    else if (estimator_type == "pessimistic_weights_pw"){
      output <- calculate_gev_inverse_cdf(p = 1 - alpha_gev_mixture_model,
                                          loc = gev_model_parameters["pessimistic_weights", "loc_star"],
                                          scale = gev_model_parameters["pessimistic_weights", "scale_star"], 
                                          shape = gev_model_parameters["pessimistic_weights", "shape_star"])
    }
    else if (estimator_type == "automatic_weights_pw"){
      output <- calculate_gev_inverse_cdf(p = 1 - alpha_gev_mixture_model,
                                          loc = gev_model_parameters["automatic_weights", "loc_star"],
                                          scale = gev_model_parameters["automatic_weights", "scale_star"], 
                                          shape = gev_model_parameters["automatic_weights", "shape_star"])
    }
    else if (estimator_type == "identic_weights_mw"){
      output <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_gev_mixture_model, 
                                                        locations = gev_mixture_model_parameters_object$loc_star, 
                                                        scales = gev_mixture_model_parameters_object$scale_star, 
                                                        shapes = gev_mixture_model_parameters_object$shape_star, 
                                                        weights = gev_mixture_model_weights_object[, "identic_weights"],
                                                        iterations = 100)
    }
    else if (estimator_type == "pessimistic_weights_mw"){
      output <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_gev_mixture_model, 
                                                        locations = gev_mixture_model_parameters_object$loc_star, 
                                                        scales = gev_mixture_model_parameters_object$scale_star, 
                                                        shapes = gev_mixture_model_parameters_object$shape_star, 
                                                        weights = gev_mixture_model_weights_object[, "pessimistic_weights"],
                                                        iterations = 100)
    }
    else if (estimator_type == "automatic_weights_mw"){
      output <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_gev_mixture_model, 
                                                        locations = gev_mixture_model_parameters_object$loc_star, 
                                                        scales = gev_mixture_model_parameters_object$scale_star, 
                                                        shapes = gev_mixture_model_parameters_object$shape_star, 
                                                        weights = gev_mixture_model_weights_object[, "automatic_weights"],
                                                        iterations = 100)
    }
    else if (estimator_type == "empirical"){
      quantile_alpha <- quantile(x = raw_data, probs = 1 - alpha)
      
      output <- as.numeric(quantile_alpha)
    }
    else if (estimator_type == "gev_model_quantiles_mw"){
      # extract the vector of weights
      weights = gev_mixture_model_weights_object[, "automatic_weights"]
      
      # get the positions where weights are different from zero
      position_weights_nonzero <- which(weights > 0)
      
      # extract all parameters for which weights are different from zero
      block_sizes <- block_sizes[position_weights_nonzero]
      
      quantiles_object <- sapply(1:length(block_sizes), function(j){
        block_size <- block_sizes[j]
        
        loc_star = gev_mixture_model_parameters_object[as.character(block_size), "loc_star"] 
        scale_star = gev_mixture_model_parameters_object[as.character(block_size), "scale_star"]
        shape_star = gev_mixture_model_parameters_object[as.character(block_size), "shape_star"]
        
        out <- calculate_gev_inverse_cdf(p = 1 - alpha_prime*block_size,
                                         loc = loc_star,
                                         scale = scale_star, 
                                         shape = shape_star)
        
        out})
      
      output <- data.frame(quantiles = quantiles_object)
      
      rownames(output) <- block_sizes
    }
    else{
      quantiles_object <- sapply(1:length(block_sizes), function(j){
        block_size <- block_sizes[j]
        
        loc_star = gev_mixture_model_parameters_object[as.character(block_size), "loc_star"] 
        scale_star = gev_mixture_model_parameters_object[as.character(block_size), "scale_star"]
        shape_star = gev_mixture_model_parameters_object[as.character(block_size), "shape_star"]
        
        out <- calculate_gev_inverse_cdf(p = 1 - alpha_prime*block_size,
                                         loc = loc_star,
                                         scale = scale_star, 
                                         shape = shape_star)
        
        out})
      
      output <- data.frame(quantiles = quantiles_object)
      
      rownames(output) <- block_sizes
    }
  }
  else{
    stop("Please, enter a smaller quantile order")
  }
  
  output
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
#                      "pessimistic_weights_mw",
#                      "identic_weights_mw",
#                      "automatic_weights_pw",
#                      "pessimistic_weights_pw",
#                      "identic_weights_pw",
#                      "empirical",
#                      "gev_model_quantiles_mw",
#                      "gev_model_quantiles_pw")
# 
# 
# alpha <- 10^(-14)
# 
# results_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                   alpha = alpha,
#                                                   estimator_type = estimator_types[1])
# 
# results_mw
# 
# results_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                   alpha = alpha,
#                                                   estimator_type = estimator_types[4])
# 
# results_pw
# 
# true_rl <- qnorm(p = 1 - alpha)
# true_rl
# 
# results_emp <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                  alpha = alpha,
#                                                  estimator_type = estimator_types[7])
# 
# results_emp
# 
# est_rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                  alpha = alpha,
#                                                  estimator_type = estimator_types[9])
# 
# est_rl_pw
# 
# est_rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                  alpha = alpha,
#                                                  estimator_type = estimator_types[8])
# 
# est_rl_mw
# 
# matplot(rownames(x = est_rl_pw),
#         y = est_rl_pw,
#         ylim = range(c(est_rl_pw, results_pw, true_rl)),
#         type = "l",
#         lty = "solid",
#         lwd = 2,
#         col = 3)
# 
# abline(h = true_rl, col = 4, lwd = 2)
# abline(h = results_mw, col = 7, lwd = 2)
# abline(h = results_pw, col = 6, lwd = 2)
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
