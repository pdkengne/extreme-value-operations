# library(BB)

source("./src/calculate_gev_mixture_model_cdf.R")
source("./src/estimate_gev_mixture_model_pessimistic_weights")
source("./src/estimate_gev_mixture_model_identic_weights.R")
source("./src/find_threshold_associated_with_given_block_size.R")

estimate_gev_mixture_model_automatic_weights <- function(gev_models){
  # gev_models: an object associated with a result of the function "estimate_several_gev_models()"
  
  # create an empty output object
  output <- list()
  
  # get the normalized gev parameters
  normalized_gev_parameters <- gev_models$normalized_gev_parameters_object
  shapes <- normalized_gev_parameters$shape_star
  scales <- normalized_gev_parameters$scale_star
  locations <- normalized_gev_parameters$loc_star
  
  # extract large data to use
  x <- gev_models$data
  block_size <- max(gev_models$block_sizes)
  threshold <- find_threshold_associated_with_given_block_size(x, block_size)
  y <- x[x > threshold]
  
  # estimate the empirical distribution function
  Fn <- ecdf(x)
  
  # get the number of gev models
  p <- nrow(normalized_gev_parameters)
  
  # define the constraints on the unknown weights
  lower <- rep(0, p - 1)
  upper <- rep(1, p - 1)
  
  # define the function to optimize
  nlf <- function(w, y){
    S <- sapply(1:length(y), function(y) {
      prob <- calculate_gev_cdf(q = y, 
                                loc = locations[j], 
                                scale = scales[j], 
                                shape = shapes[j])
      
      out <- prob^(weights[j])
      
      out
    })
    
    G <- prod(S)
    
    G
  }
  
  
  # update the output object
  output[["automatic_weights"]] <- automatic_weights
  
  output
}






















