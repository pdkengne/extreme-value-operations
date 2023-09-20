source("./src/estimate_gev_parameters.R")
source("./src/shift_data_elements_circularly.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")

estimate_gev_mixture_model_quantile <- function(alpha, x, locations, scales, shapes, weights, iterations = 100, confidence_level = 0.95){
  # alpha: order of the quantile to estimate
  # x: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # iterations: number of iterations to perform in the the dichotomy algorithm
  # confidence_level: the desired confidence level for the estimated quantile
  
  quantiles <- rep(NA, 3)
  
  quantiles[2] <- calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha, 
                                                          locations = locations, 
                                                          scales = scales, 
                                                          shapes = shapes, 
                                                          weights = weights,
                                                          iterations = iterations)
    
    # get the positions where weights are different from zero
    position_weights_nonzero <- which(weights > 0)
    
    # extract all parameters for which weights are different from zero
    locations <- locations[position_weights_nonzero]
    scales <- scales[position_weights_nonzero]
    shapes <- shapes[position_weights_nonzero]
    weights <- weights[position_weights_nonzero]
    block_sizes <- gev_mixture_model$block_sizes[position_weights_nonzero]
    
    names(gev_mixture_model)
    
    gev_models_object <- gev_mixture_model$gev_models_object
    
    gev_models_object[[block_sizes[1]]]
    
    
    # calculates some initial guesses for the root of the nonlinear equation to solve
    marginal_quantiles_ci <- sapply(1:length(weights), function(j){
      block_size <- gev_mixture_model$block_sizes[j]
      threshold <- find_threshold_associated_with_given_block_size(x = uvdata, block_size = block_size)
      tau <- mean(raw_data > threshold)
      alpha_prime <- alpha/tau

      model <- estimate_gev_parameters(x = gev_models_object[[j]]$data, 
                                       nsloc = NULL,
                                       scale = scales[j], 
                                       shape = shapes[j], 
                                       prob = alpha_prime,
                                       std.err = TRUE)
      
      quantiles_ci <- rep(NA, 3)
      
      quantiles_ci[2] <- model$estimate
      
      quantiles_ci[c(1,3)] <- as.numeric(confint(object = model, level = confidence_level))
      
      names(quantiles_ci) <- c("lower", "estimate", "upper")
      
      quantiles_ci
  })
  
  output
}



