source("./src/estimate_gev_parameters.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")

calculate_gev_mixture_model_quantile <- function(gev_mixture_model, 
                                                 type = NULL,
                                                 model_wise = FALSE,
                                                 alpha = NULL,
                                                 confidence_level = 0.95){
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_mixture_model_parameters()"
  # type: type of gev mixture model to consider. It is one of the following elements
  # model_wise: a boolean which indicates whether to use weights on models or on parameters
  #       ("identic_weights_pw", "pessimistic_weights_pw", "automatic_weights_pw",
  #        "identic_weights_mw", "pessimistic_weights_mw", "automatic_weights_mw").
  # alpha: order of the quantile to estimate
  # confidence_level: the desired confidence level for the estimated quantile
  
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
    
    quantiles <- data.frame(t(quantiles))
    
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

    # calculate quantile
    if (is.element(el = type, set = weighted_gev_model_types) & !model_wise){
      quantiles <- rep(NA, 3)
      names(quantiles) <- c("lower", "estimate", "upper")
      quantiles[2] <-  calculate_gev_inverse_cdf(p = 1 - alpha_prime,
                                                 loc = gev_model_parameters[type, "loc_star"],
                                                 scale = gev_model_parameters[type, "scale_star"], 
                                                 shape = gev_model_parameters[type, "shape_star"])
    }
    
    if (is.element(el = type, set = weighted_gev_model_types) & model_wise){
      quantiles <- rep(NA, 3)
      names(quantiles) <- c("lower", "estimate", "upper")
      quantiles[2] <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha_prime, 
                                                              locations = gev_mixture_model_parameters_object$loc_star, 
                                                              scales = gev_mixture_model_parameters_object$scale_star, 
                                                              shapes = gev_mixture_model_parameters_object$shape_star, 
                                                              weights = gev_mixture_model_weights_object[, type],
                                                              iterations = 100)
      
      # extract the vector of block sizes
      block_sizes <- gev_mixture_model$block_sizes
      
      # extract the list of all estimated gev models
      gev_models_objects <- gev_mixture_model$gev_models_object
      
      # extract the vector of weights
      weights = gev_mixture_model_weights_object[, type]
      
      # get the positions where weights are different from zero
      position_weights_nonzero <- which(weights > 0)
      
      # extract all parameters for which weights are different from zero
      block_sizes <- block_sizes[position_weights_nonzero]
      gev_models_objects <- gev_models_objects[position_weights_nonzero]
      
      if (max(block_sizes)*alpha_prime < 1){
        quantiles_object <- sapply(1:length(block_sizes), function(j){
          block_size <- block_sizes[j]
          
          gev_model <- gev_models_objects[[j]]
          
          maxima <- gev_model$data
          
          model <- estimate_gev_parameters(x = maxima, prob = alpha_prime*block_size, std.err = TRUE)
          
          ci <- confint(model, level = confidence_level)
          
          ci["quantile", ]})
      }
      else{
        print("Please, enter a smaller quantile order")
      }
      
      
      
      quantiles[c(1, 3)] <- range(quantiles_object)
    }
    
  }
  
  quantiles
}



# example 1

source("./src/generate_gev_sample.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/estimate_gev_mixture_model_parameters.R")

n <- 10000
nlargest <- 1000

tau <- nlargest/n

loc <- 1
scale <- 0.5
shape <- 0.1

# x <- rnorm(n = n)
x <- generate_gev_sample(n = n, loc = loc, scale = scale, shape = shape)
x <- rnorm(n = n)

gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
                                                           nsloc = NULL,
                                                           std.err = FALSE,
                                                           block_sizes = NULL,
                                                           minimum_nblocks = 50,
                                                           nlargest = nlargest,
                                                           confidence_level = 0.95,
                                                           trace = TRUE)

# set the types of weighted gev models
weighted_gev_model_types = c("identic_weights", "pessimistic_weights", "automatic_weights")


alpha <- 0.00002


results <- calculate_gev_mixture_model_quantile(gev_mixture_model, 
                                                type = weighted_gev_model_types[1],
                                                model_wise = TRUE,
                                                alpha = alpha,
                                                confidence_level = 0.95)

results

quantile(x = x, probs = 1 - alpha)

calculate_gev_inverse_cdf(p = 1 - alpha, loc = loc, scale = scale, shape = shape)

qnorm(p = 1 - alpha)



