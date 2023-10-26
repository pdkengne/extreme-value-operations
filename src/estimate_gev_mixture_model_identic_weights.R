estimate_gev_mixture_model_identic_weights <- function(gev_models, use_extremal_index = TRUE){
  # gev_models: an object associated with a result of the function "estimate_several_gev_models()"
  # use_extremal_index: a boolean which indicates whether to use the estimates extremal indexes or not
  
  # get the normalized gev parameters
  if (use_extremal_index){
    normalized_gev_parameters <- gev_models$full_normalized_gev_parameters_object
  }
  else{
    normalized_gev_parameters <- gev_models$normalized_gev_parameters_object
  }

  # get the number of gev models
  p <- nrow(normalized_gev_parameters)
  
  # calculate the identic weights
  identic_weights <- rep(1, p)/p
  names(identic_weights) <- rownames(normalized_gev_parameters)
  
  identic_weights
}


# # example 1
# 
# source("./src/estimate_several_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/estimate_several_standardized_block_maxima_mean.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = -0.2)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# block_sizes
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes, nsloc = NULL)
# 
# results <- estimate_gev_mixture_model_identic_weights(gev_models, use_extremal_index = TRUE)
# 
# results
# range(results)
# sum(results)

