source("./src/estimate_gev_parameters.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")

calculate_gev_mixture_model_quantile <- function(gev_mixture_model, alpha = NULL, confidence_level = 0.95){
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_mixture_model_parameters()"
  # alpha: order of the quantile to estimate
  # confidence_level: the desired confidence level for the estimated quantile
  
  # create an empty output object
  output <- list()
  
  # initialized the output content
  out <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
  
  estimated_empirical_quantile <- out
  
  estimated_identic_weighted_gev_mixture_model_quantile_pw <- out
  
  estimated_pessimistic_weighted_gev_mixture_model_quantile_pw <- out
  
  estimated_automatic_weighted_gev_mixture_model_quantile_pw <- out
  
  estimated_identic_weighted_gev_mixture_model_quantile_mw <- out
  
  estimated_pessimistic_weighted_gev_mixture_model_quantile_mw <- out
  
  estimated_automatic_weighted_gev_mixture_model_quantile_mw <- out
  
  estimated_gev_model_quantile_unrestricted_weights <- out
  
  estimated_gev_model_quantile_restricted_weights <- out
  
  # extract the raw data
  raw_data <- gev_mixture_model$data
  
  # extract the largest train data
  data_largest <- gev_mixture_model$data_largest
  
  # calculate the proportion of largest data
  tau <- length(data_largest)/length(raw_data)
          
  # set the appropriate quantile order
  alpha_prime <- alpha/tau
  
  # calculate the required quantile
  if (alpha_prime >= 1){
    
    threshold_alpha <- quantile(x = raw_data, probs = 1 - alpha)
    
    k_alpha <- sum(raw_data > threshold_alpha)
    
    test <- binom.test(x = k_alpha, n = n, conf.level = confidence_level)
    
    probs <- 1 - c(test$conf.int[2], test$estimate, test$conf.int[1])
    
    quantiles <- as.numeric(quantile(x = raw_data, probs = probs))
    
    names(quantiles) <- c("lower", "estimate", "upper")
    
    estimated_empirical_quantile <- data.frame(t(quantiles))
    
  }
  else{
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
    
    if (max(block_sizes)*alpha_prime < 1){
      quantiles_object <- sapply(1:length(block_sizes), function(j){
        block_size <- block_sizes[j]
        
        gev_model <- gev_models_object[[j]]
        
        maxima <- gev_model$data
        
        model <- estimate_gev_parameters(x = maxima, prob = alpha_prime*block_size, std.err = TRUE)
        
        ci <- confint(model, level = confidence_level)
        
        out <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
        
        out[2] <- model$estimate["quantile"]
        
        out[c(1, 3)] <- ci["quantile", ]
        
        out})
      
      estimated_gev_model_quantile_unrestricted_weights <- data.frame(t(quantiles_object))
      
      rownames(estimated_gev_model_quantile_unrestricted_weights) <- block_sizes
       
    }
    else{
      print("Please, enter a smaller quantile order")
    }
    
    # estimate quantiles associated with gev mixture model (pw): identic weights
    quantiles <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
    quantiles[2] <-  calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                               loc = gev_model_parameters["identic_weights", "loc_star"],
                                               scale = gev_model_parameters["identic_weights", "scale_star"], 
                                               shape = gev_model_parameters["identic_weights", "shape_star"])
    
    quantiles[c(1, 3)] <- range(as.matrix(estimated_gev_model_quantile_unrestricted_weights))
    
    estimated_identic_weighted_gev_mixture_model_quantile_pw <- quantiles
    
    # estimate quantiles associated with gev mixture model (pw): pessimistic weights
    quantiles <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
    quantiles[2] <-  calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                               loc = gev_model_parameters["pessimistic_weights", "loc_star"],
                                               scale = gev_model_parameters["pessimistic_weights", "scale_star"], 
                                               shape = gev_model_parameters["pessimistic_weights", "shape_star"])
    
    quantiles[c(1, 3)] <- range(as.matrix(estimated_gev_model_quantile_unrestricted_weights))
    
    estimated_pessimistic_weighted_gev_mixture_model_quantile_pw <- quantiles
    
    # estimate quantiles associated with gev mixture model (pw): automatic weights
    quantiles <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
    quantiles[2] <-  calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                               loc = gev_model_parameters["automatic_weights", "loc_star"],
                                               scale = gev_model_parameters["automatic_weights", "scale_star"], 
                                               shape = gev_model_parameters["automatic_weights", "shape_star"])
    
    quantiles[c(1, 3)] <- range(as.matrix(estimated_gev_model_quantile_unrestricted_weights))
    
    estimated_automatic_weighted_gev_mixture_model_quantile_pw <- quantiles
    
    # estimate quantiles associated with gev mixture model (mw): identic weights
    quantiles <- rep(NA, 3)
    names(quantiles) <- c("lower", "estimate", "upper")
    quantiles[2] <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                            locations = gev_mixture_model_parameters_object$loc_star, 
                                                            scales = gev_mixture_model_parameters_object$scale_star, 
                                                            shapes = gev_mixture_model_parameters_object$shape_star, 
                                                            weights = gev_mixture_model_weights_object[, "identic_weights"],
                                                            iterations = 100)
    
    quantiles[c(1, 3)] <- range(as.matrix(estimated_gev_model_quantile_unrestricted_weights))
    
    estimated_identic_weighted_gev_mixture_model_quantile_mw <- quantiles
    
    # estimate quantiles associated with gev mixture model (mw): pessimistic weights
    quantiles <- rep(NA, 3)
    names(quantiles) <- c("lower", "estimate", "upper")
    quantiles[2] <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                            locations = gev_mixture_model_parameters_object$loc_star, 
                                                            scales = gev_mixture_model_parameters_object$scale_star, 
                                                            shapes = gev_mixture_model_parameters_object$shape_star, 
                                                            weights = gev_mixture_model_weights_object[, "pessimistic_weights"],
                                                            iterations = 100)
    
    quantiles[c(1, 3)] <- range(as.matrix(estimated_gev_model_quantile_unrestricted_weights))
    
    estimated_pessimistic_weighted_gev_mixture_model_quantile_mw <- quantiles
    
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
        
        maxima <- gev_model$data
        
        model <- estimate_gev_parameters(x = maxima, prob = alpha_prime*block_size, std.err = TRUE)
        
        ci <- confint(model, level = confidence_level)
        
        out <- data.frame("lower" = NA, "estimate" = NA, "upper" = NA)
        
        out[2] <- model$estimate["quantile"]
        
        out[c(1, 3)] <- ci["quantile", ]
        
        out})
      
      estimated_gev_model_quantile_restricted_weights <- data.frame(t(quantiles_object))
      
      rownames(estimated_gev_model_quantile_restricted_weights) <- block_sizes
    }
    else{
      print("Please, enter a smaller quantile order")
    }
    
    # estimate quantiles associated with gev mixture model (mw): automatic weights
    quantiles <- rep(NA, 3)
    names(quantiles) <- c("lower", "estimate", "upper")
    quantiles[2] <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                            locations = gev_mixture_model_parameters_object$loc_star, 
                                                            scales = gev_mixture_model_parameters_object$scale_star, 
                                                            shapes = gev_mixture_model_parameters_object$shape_star, 
                                                            weights = gev_mixture_model_weights_object[, "automatic_weights"],
                                                            iterations = 100)
    
    quantiles[c(1, 3)] <- range(as.matrix(estimated_gev_model_quantile_restricted_weights))
    
    estimated_automatic_weighted_gev_mixture_model_quantile_mw <- quantiles
 
  }   
    
  # update the output object
  output[["estimated_empirical_quantile"]] <- estimated_empirical_quantile
  output[["estimated_identic_weighted_gev_mixture_model_quantile_pw"]] <- estimated_identic_weighted_gev_mixture_model_quantile_pw
  output[["estimated_pessimistic_weighted_gev_mixture_model_quantile_pw"]] <- estimated_pessimistic_weighted_gev_mixture_model_quantile_pw
  output[["estimated_automatic_weighted_gev_mixture_model_quantile_pw"]] <- estimated_automatic_weighted_gev_mixture_model_quantile_pw
  output[["estimated_identic_weighted_gev_mixture_model_quantile_mw"]] <- estimated_identic_weighted_gev_mixture_model_quantile_mw
  output[["estimated_pessimistic_weighted_gev_mixture_model_quantile_mw"]] <- estimated_pessimistic_weighted_gev_mixture_model_quantile_mw
  output[["estimated_automatic_weighted_gev_mixture_model_quantile_mw"]] <- estimated_automatic_weighted_gev_mixture_model_quantile_mw
  output[["estimated_gev_model_quantile_unrestricted_weights"]] <- estimated_gev_model_quantile_unrestricted_weights
  output[["estimated_gev_model_quantile_restricted_weights"]] <- estimated_gev_model_quantile_restricted_weights
  
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
#                                                            nsloc = NULL,
#                                                            std.err = FALSE,
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
# 
# alpha <- 10^(-6)
# 
# results <- calculate_gev_mixture_model_quantile(gev_mixture_model, 
#                                                 alpha = alpha,
#                                                 confidence_level = 0.95)
# 
# results
# 
# quantile(x = x, probs = 1 - alpha)
# 
# qnorm(p = 1 - alpha)
# 
# 
# true_rl <- qnorm(p = 1 - alpha)
# 
# est_rl <- results$estimated_gev_model_quantile_unrestricted_weight
# 
# 
# matplot(rownames(est_rl), est_rl, type = "l", lty = "dotted")
# 
# abline(h = true_rl, col = 4)
# abline(h = results$estimated_automatic_weighted_gev_mixture_model_quantile_pw, col = 5)
# abline(h = results$estimated_automatic_weighted_gev_mixture_model_quantile_mw, col = 6)
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
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# source("./src/calculate_gev_inverse_cdf.R")
# source("./src/estimate_gev_mixture_model_parameters.R")
# 
# n <- 100000
# nlargest <- 1000
# 
# loc <- 1
# scale <- 0.5
# shape <- 0.1
# 
# x <- generate_gev_sample(n = n, loc = loc, scale = scale, shape = shape)
# 
# gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
#                                                            nsloc = NULL,
#                                                            std.err = FALSE,
#                                                            block_sizes = NULL,
#                                                            minimum_nblocks = 50,
#                                                            nlargest = nlargest,
#                                                            confidence_level = 0.95,
#                                                            trace = TRUE)
# 
# gev_mixture_model$automatic_weights_mw
# 
# gev_mixture_model$normalized_gev_parameters_object
# 
# gev_mixture_model$weighted_normalized_gev_parameters_object
# 
# gev_mixture_model$automatic_weights_mw_statistics
# 
# gev_mixture_model$automatic_weights_pw_statistics
# 
# 
# 
# alpha <- 10^(-6)
# 
# results <- calculate_gev_mixture_model_quantile(gev_mixture_model, 
#                                                 alpha = alpha,
#                                                 confidence_level = 0.95)
# 
# results
# 
# quantile(x = x, probs = 1 - alpha)
# 
# true_rl <- calculate_gev_inverse_cdf(p = 1 - alpha, loc = loc, scale = scale, shape = shape)
# 
# est_rl <- results$estimated_gev_model_quantile_unrestricted_weight
# 
# 
# matplot(rownames(est_rl), est_rl, type = "l", lty = "dotted")
# 
# abline(h = true_rl, col = 4)
# abline(h = results$estimated_automatic_weighted_gev_mixture_model_quantile_pw, col = 5)
# abline(h = results$estimated_automatic_weighted_gev_mixture_model_quantile_mw, col = 6)
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
