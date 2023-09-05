estimate_gev_mixture_model_pessimistic_weights <- function(gev_models){
  # gev_models: an object associated with a result of the function "estimate_several_gev_models()"
  
  # create an empty output object
  output <- list()
  
  # get the considered sizes of blocks
  block_sizes <- gev_models$block_sizes
  
  # get the normalized gev parameters
  normalized_gev_parameters <- gev_models$normalized_gev_parameters_object
  
  # calculate the pessimistic weights associated with the shape parameter
  normalized_gev_parameters_shape <- normalized_gev_parameters$shape_star
  normalized_gev_parameters_shape_cdf <- ecdf(normalized_gev_parameters_shape)
  normalized_gev_parameters_shape_cdf_estimates <- normalized_gev_parameters_shape_cdf(normalized_gev_parameters_shape)
  pessimistic_weights_shape <- normalized_gev_parameters_shape_cdf_estimates/sum(normalized_gev_parameters_shape_cdf_estimates)
  names(pessimistic_weights_shape) <- block_sizes
  
  # calculate the pessimistic weights associated with the scale parameter
  normalized_gev_parameters_scale <- normalized_gev_parameters$scale_star
  normalized_gev_parameters_scale_cdf <- ecdf(normalized_gev_parameters_scale)
  normalized_gev_parameters_scale_cdf_estimates <- normalized_gev_parameters_scale_cdf(normalized_gev_parameters_scale)
  pessimistic_weights_scale <- normalized_gev_parameters_scale_cdf_estimates/sum(normalized_gev_parameters_scale_cdf_estimates)
  names(pessimistic_weights_scale) <- block_sizes
  
  # calculate the pessimistic weights associated with the location parameter
  normalized_gev_parameters_loc <- normalized_gev_parameters$loc_star
  normalized_gev_parameters_loc_cdf <- ecdf(normalized_gev_parameters_loc)
  normalized_gev_parameters_loc_cdf_estimates <- normalized_gev_parameters_loc_cdf(normalized_gev_parameters_loc)
  pessimistic_weights_loc <- normalized_gev_parameters_loc_cdf_estimates/sum(normalized_gev_parameters_loc_cdf_estimates)
  names(pessimistic_weights_loc) <- block_sizes
  
  # calculate the pessimistic weights associated with the considered gev models
  pessimistic_weights_models_object <- cbind(normalized_gev_parameters_shape_cdf_estimates,
                                             normalized_gev_parameters_scale_cdf_estimates,
                                             normalized_gev_parameters_loc_cdf_estimates)
  
  pessimistic_weights_models <- apply(pessimistic_weights_models_object, 1, max)
  pessimistic_weights <- pessimistic_weights_models/sum(pessimistic_weights_models)
  names(pessimistic_weights) <- block_sizes
  
  # update the output object
  output[["pessimistic_weights"]] <- pessimistic_weights
  output[["pessimistic_weights_shape"]] <- pessimistic_weights_shape
  output[["pessimistic_weights_scale"]] <- pessimistic_weights_scale
  output[["pessimistic_weights_loc"]] <- pessimistic_weights_loc
  
  output
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
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes, nsloc = NULL)
# 
# results <- estimate_gev_mixture_model_pessimistic_weights(gev_models)
# 
# # results
# names(results)
# lapply(results, function(weights) range(weights))
# lapply(results, function(weights) sum(weights))
# 
# y_df <- cbind(results$pessimistic_weights_shape,
#               results$pessimistic_weights_scale,
#               results$pessimistic_weights_loc,
#               results$pessimistic_weights)
# 
# y <- apply(y_df, 1, max)
# 
# plot(names(results$pessimistic_weights_shape), y, ylim = range(y_df),
#      type = "h", xlab = "block sizes", ylab = "weights", 
#      main = "estimated weights: shape (blue), scale (green), location (red) and model (yellow)")
# 
# points(names(results$pessimistic_weights_shape), results$pessimistic_weights_shape, col = 4, pch = 20)
# 
# points(names(results$pessimistic_weights_scale), results$pessimistic_weights_scale, col = 3, pch = 20)
# 
# points(names(results$pessimistic_weights_loc), results$pessimistic_weights_loc, col = 2, pch = 20)
# 
# points(names(results$pessimistic_weights_loc), results$pessimistic_weights, col = 7, pch = 20)
# 
# 
# # example 2
# 
# source("./src/estimate_several_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/estimate_several_standardized_block_maxima_mean.R")
# 
# x <- rnorm(n = 10000)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes, nsloc = NULL)
# 
# results <- estimate_gev_mixture_model_pessimistic_weights(gev_models)
# 
# # results
# names(results)
# lapply(results, function(weights) range(weights))
# lapply(results, function(weights) sum(weights))
# 
# y_df <- cbind(results$pessimistic_weights_shape,
#               results$pessimistic_weights_scale,
#               results$pessimistic_weights_loc,
#               results$pessimistic_weights)
# 
# y <- apply(y_df, 1, max)
# 
# plot(names(results$pessimistic_weights_shape), y, ylim = range(y_df),
#      type = "h", xlab = "block sizes", ylab = "weights",
#      main = "estimated weights: shape (blue), scale (green), location (red) and model (yellow)")
# 
# points(names(results$pessimistic_weights_shape), results$pessimistic_weights_shape, col = 4, pch = 20)
# 
# points(names(results$pessimistic_weights_scale), results$pessimistic_weights_scale, col = 3, pch = 20)
# 
# points(names(results$pessimistic_weights_loc), results$pessimistic_weights_loc, col = 2, pch = 20)
# 
# points(names(results$pessimistic_weights_loc), results$pessimistic_weights, col = 7, pch = 20)
