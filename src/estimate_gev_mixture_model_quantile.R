# library(EnvStats)

source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")
source("./src/estimate_gev_parameters.R")
source("./src/estimate_gev_model_quantile.R")

estimate_gev_mixture_model_quantile <- function(gev_mixture_model,
                                                kind = c("geometric", "arithmetic")[1],
                                                alpha,
                                                do.ci = TRUE,
                                                confidence_level = 0.95,
                                                estimator_type = c("automatic_weights_mw", 
                                                                   "pessimistic_weights_mw", 
                                                                   "identic_weights_mw", 
                                                                   "automatic_weights_pw",
                                                                   "pessimistic_weights_pw", 
                                                                   "identic_weights_pw", 
                                                                   "model_wise",
                                                                   "parameter_wise",
                                                                   "empirical")[1]){
  # gev_mixture_model: an object associated with a result of the function 
  #                    "estimate_gev_mixture_model_parameters()" or "predict_gev_mixture_model_parameters()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  # alpha: order of the quantile to estimate
  # do.ci: boolean which indicates whether to return confidence interval or not
  # confidence_level: the desired confidence level for the estimated quantile
  # estimator_type: quantile estimator to use from the set 
  # c("automatic_weights_mw", "pessimistic_weights_mw", "identic_weights_mw", "automatic_weights_pw","pessimistic_weights_pw", 
  #   "identic_weights_pw", "model_wise", "parameter_wise", "empirical")
  
  # extract the raw data
  raw_data <- gev_mixture_model$data
  
  # extract the used largest dataset
  data_largest <- gev_mixture_model$data_largest
  
  # extract the vector of block sizes
  block_sizes <- gev_mixture_model$block_sizes
  
  # get the location indicator of the threshold
  use_lower_threshold <- gev_mixture_model$use_lower_threshold
  
  # find threshold above which all fitted models are well defined
  if (use_lower_threshold){
    threshold <- find_threshold_associated_with_given_block_size(x = data_largest, 
                                                                 block_size = min(block_sizes))
  }
  else{
    threshold <- find_threshold_associated_with_given_block_size(x = data_largest, 
                                                                 block_size = max(block_sizes))
  }
  
  # calculate the proportion of data which exceed the threshold
  tau <- length(data_largest)/length(raw_data)
  # tau <- sum(raw_data > threshold)/length(raw_data)
  
  # calculate the quantile order to use for all fitted models
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
  
  # extract the list of all estimated gev models
  gev_models_object <- gev_mixture_model$gev_models_object
  
  if (alpha_prime < 1){
    if (estimator_type == "identic_weights_pw"){
      output <- calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                          loc = gev_model_parameters["identic_weights", "loc_star"],
                                          scale = gev_model_parameters["identic_weights", "scale_star"], 
                                          shape = gev_model_parameters["identic_weights", "shape_star"])
    }
    else if (estimator_type == "pessimistic_weights_pw"){
      output <- calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                          loc = gev_model_parameters["pessimistic_weights", "loc_star"],
                                          scale = gev_model_parameters["pessimistic_weights", "scale_star"], 
                                          shape = gev_model_parameters["pessimistic_weights", "shape_star"])
    }
    else if (estimator_type == "automatic_weights_pw"){
      output <- calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                          loc = gev_model_parameters["automatic_weights", "loc_star"],
                                          scale = gev_model_parameters["automatic_weights", "scale_star"], 
                                          shape = gev_model_parameters["automatic_weights", "shape_star"])
    }
    else if (estimator_type == "identic_weights_mw"){
      output <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                        locations = gev_mixture_model_parameters_object$loc_star, 
                                                        scales = gev_mixture_model_parameters_object$scale_star, 
                                                        shapes = gev_mixture_model_parameters_object$shape_star, 
                                                        weights = gev_mixture_model_weights_object[, "identic_weights"],
                                                        kind = kind,
                                                        iterations = 100)
    }
    else if (estimator_type == "pessimistic_weights_mw"){
      output <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                        locations = gev_mixture_model_parameters_object$loc_star, 
                                                        scales = gev_mixture_model_parameters_object$scale_star, 
                                                        shapes = gev_mixture_model_parameters_object$shape_star, 
                                                        weights = gev_mixture_model_weights_object[, "pessimistic_weights"],
                                                        kind = kind,
                                                        iterations = 100)
    }
    else if (estimator_type == "automatic_weights_mw"){
      output <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                        locations = gev_mixture_model_parameters_object$loc_star, 
                                                        scales = gev_mixture_model_parameters_object$scale_star, 
                                                        shapes = gev_mixture_model_parameters_object$shape_star, 
                                                        weights = gev_mixture_model_weights_object[, "automatic_weights"],
                                                        kind = kind,
                                                        iterations = 100)
    }
    else if (estimator_type == "model_wise"){
      # extract the vector of weights
      weights = gev_mixture_model_weights_object[, "automatic_weights"]
      
      # get the positions where weights are different from zero
      position_weights_nonzero <- which(weights > 0)
      
      # extract all parameters for which weights are different from zero
      block_sizes <- block_sizes[position_weights_nonzero]
      gev_models_object <- gev_models_object[position_weights_nonzero]
      
      quantiles_object <- sapply(1:length(block_sizes), function(j){
        block_size <- block_sizes[j]
        gev_model <- gev_models_object[[j]]
        
        loc_star = gev_mixture_model_parameters_object[as.character(block_size), "loc_star"] 
        scale_star = gev_mixture_model_parameters_object[as.character(block_size), "scale_star"]
        shape_star = gev_mixture_model_parameters_object[as.character(block_size), "shape_star"]
        
        gev_parameters <- gev_model$results$par
        
        loc <- gev_parameters["location"]
        scale <- gev_parameters["scale"]
        shape <- gev_parameters["shape"]
        
        block_maxima <- gev_model$x
        
        block_maxima_standardized <- (block_maxima - loc)/scale
        
        block_maxima_star <- loc_star + scale_star*block_maxima_standardized
        
        gev_model_star <- estimate_gev_parameters(x = block_maxima_star,
                                                  type = c("GEV", "Gumbel")[1],
                                                  method = c("MLE", "GMLE", "Lmoments")[1])
        
        out <- estimate_gev_model_quantile(gev_model = gev_model_star,
                                           alpha = alpha_prime,
                                           do.ci = do.ci,
                                           confidence_level = confidence_level)
        
        out})
      
      output <- data.frame(t(quantiles_object))
      rownames(output) <- block_sizes
      names(output) <- c("lower", "quantile", "upper")
    }
    else if (estimator_type == "parameter_wise"){
      quantiles_object <- sapply(1:length(block_sizes), function(j){
        block_size <- block_sizes[j]
        gev_model <- gev_models_object[[j]]
        
        loc_star = gev_mixture_model_parameters_object[as.character(block_size), "loc_star"] 
        scale_star = gev_mixture_model_parameters_object[as.character(block_size), "scale_star"]
        shape_star = gev_mixture_model_parameters_object[as.character(block_size), "shape_star"]
        
        gev_parameters <- gev_model$results$par
        
        loc <- gev_parameters["location"]
        scale <- gev_parameters["scale"]
        shape <- gev_parameters["shape"]
        
        block_maxima <- gev_model$x
        
        block_maxima_standardized <- (block_maxima - loc)/scale
        
        block_maxima_star <- loc_star + scale_star*block_maxima_standardized
        
        gev_model_star <- estimate_gev_parameters(x = block_maxima_star,
                                                  type = c("GEV", "Gumbel")[1],
                                                  method = c("MLE", "GMLE", "Lmoments")[1])
        
        out <- estimate_gev_model_quantile(gev_model = gev_model_star,
                                           alpha = alpha_prime,
                                           do.ci = do.ci,
                                           confidence_level = confidence_level)
        
        out})
      
      output <- data.frame(t(quantiles_object))
      rownames(output) <- block_sizes
      names(output) <- c("lower", "quantile", "upper")
    }
    else{
      output <- EnvStats::qemp(p = 1 - alpha, obs = raw_data, prob.method = "emp.probs")
    }
  }
  else{
    output <- EnvStats::qemp(p = 1 - alpha, obs = raw_data, prob.method = "emp.probs")
  }
  
  output
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
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
#                      "model_wise",
#                      "parameter_wise",
#                      "empirical")
# 
# 
# alpha <- 10^(-14)
# 
# results_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                   kind = c("geometric", "arithmetic")[1],
#                                                   alpha = alpha,
#                                                   do.ci = TRUE,
#                                                   confidence_level = 0.95,
#                                                   estimator_type = estimator_types[1])
# 
# results_mw
# 
# results_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                   kind = c("geometric", "arithmetic")[1],
#                                                   alpha = alpha,
#                                                   do.ci = TRUE,
#                                                   confidence_level = 0.95,
#                                                   estimator_type = estimator_types[4])
# 
# results_pw
# 
# true_rl <- qnorm(p = 1 - alpha)
# true_rl
# 
# results_emp <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                    kind = c("geometric", "arithmetic")[1],
#                                                    alpha = alpha,
#                                                    do.ci = TRUE,
#                                                    confidence_level = 0.95,
#                                                    estimator_type = estimator_types[9])
# 
# results_emp
# 
# est_rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                  kind = c("geometric", "arithmetic")[1],
#                                                  alpha = alpha,
#                                                  do.ci = FALSE,
#                                                  confidence_level = 0.95,
#                                                  estimator_type = estimator_types[8])
# 
# est_rl_pw
# 
# est_rl_pw_ci <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                     kind = c("geometric", "arithmetic")[1],
#                                                  alpha = alpha,
#                                                  do.ci = TRUE,
#                                                  confidence_level = 0.95,
#                                                  estimator_type = estimator_types[8])
# 
# est_rl_pw_ci
# 
# est_rl_pw_ci_range <- range(est_rl_pw_ci)
# est_rl_pw_ci_range
# 
# est_rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                  kind = c("geometric", "arithmetic")[1],
#                                                  alpha = alpha,
#                                                  do.ci = FALSE,
#                                                  confidence_level = 0.95,
#                                                  estimator_type = estimator_types[7])
# 
# est_rl_mw
# 
# est_rl_mw_ci <- estimate_gev_mixture_model_quantile(gev_mixture_model,
#                                                     kind = c("geometric", "arithmetic")[1],
#                                                     alpha = alpha,
#                                                     do.ci = TRUE,
#                                                     confidence_level = 0.95,
#                                                     estimator_type = estimator_types[7])
# 
# est_rl_mw_ci
# 
# est_rl_mw_ci_range <- range(est_rl_mw_ci)
# est_rl_mw_ci_range
# 
# matplot(rownames(x = est_rl_pw),
#         y = est_rl_pw_ci,
#         ylim = range(c(est_rl_pw, results_pw, true_rl, range(est_rl_pw_ci))),
#         type = "l",
#         lty  = c("dotted", "solid", "dotted"),
#         lwd = 2,
#         col = c(3, 1, 3))
# 
# abline(h = true_rl, col = 4, lwd = 2)
# abline(h = results_mw, col = 7, lwd = 2)
# abline(h = results_pw, col = 6, lwd = 2)
# abline(h = est_rl_pw_ci_range, col = 7, lwd = 2, lty = "dotted")
# abline(h = est_rl_mw_ci_range, col = 6, lwd = 2, lty = "dotted")
# 
# 
# source("./src/plot_gev_mixture_model_pdf.R")
# 
# 
# plot_gev_mixture_model_pdf(gev_mixture_model,
#                            kind = c("geometric", "arithmetic")[1],
#                            type = "automatic_weights",
#                            model_wise = FALSE,
#                            zoom = FALSE,
#                            xlab = "Quantile",
#                            ylab = "Density",
#                            main = "Probability Density Function (PDF) Plot")
# 
# plot_gev_mixture_model_pdf(gev_mixture_model,
#                            kind = c("geometric", "arithmetic")[1],
#                            type = "automatic_weights",
#                            model_wise = FALSE,
#                            zoom = TRUE,
#                            xlab = "Quantile",
#                            ylab = "Density",
#                            main = "Probability Density Function (PDF) Plot")
# 
# 
# plot_gev_mixture_model_pdf(gev_mixture_model,
#                            kind = c("geometric", "arithmetic")[1],
#                            type = "automatic_weights",
#                            model_wise = TRUE,
#                            zoom = FALSE,
#                            xlab = "Quantile",
#                            ylab = "Density",
#                            main = "Probability Density Function (PDF) Plot")
# 
# plot_gev_mixture_model_pdf(gev_mixture_model,
#                            kind = c("geometric", "arithmetic")[2],
#                            type = "automatic_weights",
#                            model_wise = TRUE,
#                            zoom = TRUE,
#                            xlab = "Quantile",
#                            ylab = "Density",
#                            main = "Probability Density Function (PDF) Plot")
