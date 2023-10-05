source("./src/estimate_single_gev_model.R")

estimate_several_gev_models <- function(x, block_sizes){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  
  # create an empty output object
  output <- list()
  
  # estimate the gev model associated with each block size
  models <- lapply(block_sizes, 
                   function(block_size)
                     estimate_single_gev_model(x = x, block_size = block_size)) 
  
  # extract the extremal indexes
  extremal_indexes <- sapply(models, function(model) model$extremal_index)
  names(extremal_indexes) <- block_sizes
  
  # extract the normalized gev parameters with block size
  normalized_gev_parameters_object <- sapply(models, function(model) model$normalized_gev_parameters)
  normalized_gev_parameters_object <- data.frame(t(normalized_gev_parameters_object))
  rownames(normalized_gev_parameters_object) <- block_sizes
  
  # extract the normalized gev parameters with both block size and extremal index
  full_normalized_gev_parameters_object <- sapply(models, function(model) model$full_normalized_gev_parameters)
  full_normalized_gev_parameters_object <- data.frame(t(full_normalized_gev_parameters_object))
  rownames(full_normalized_gev_parameters_object) <- block_sizes
  
  # extract the block maxima 
  block_maxima_object <- lapply(models, function(model) model$block_maxima)
  names(block_maxima_object) <- block_sizes
  
  # extract the block maxima indexes
  block_maxima_indexes_object <- lapply(models, function(model) model$block_maxima_indexes)
  names(block_maxima_indexes_object) <- block_sizes
  
  # extract the gev models
  gev_models_object <- lapply(models, function(model) model$gev_model)
  names(gev_models_object) <- block_sizes
  
  # update the output object
  output[["data"]] <- x
  output[["block_sizes"]] <- block_sizes
  output[["block_maxima_object"]] <- block_maxima_object
  output[["block_maxima_indexes_object"]] <- block_maxima_indexes_object
  output[["gev_models_object"]] <- gev_models_object
  output[["extremal_indexes"]] <- extremal_indexes
  output[["normalized_gev_parameters_object"]] <- normalized_gev_parameters_object
  output[["full_normalized_gev_parameters_object"]] <- full_normalized_gev_parameters_object
  
  output
}


# # example 1
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/plot_several_standardized_block_maxima_mean.R")
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
# plot_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, equivalent = FALSE)
# plot_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, equivalent = TRUE)
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# results <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes)
# 
# #results
# names(results)
# 
# # get the block sizes
# results$block_sizes
# 
# # get the extremal indexes
# results$extremal_indexes
# 
# # get the associated block maxima
# results$block_maxima_object
# 
# # get the associated block maxima indexes
# results$block_maxima_indexes_object
# 
# # get the normalized gev parameters
# results$normalized_gev_parameters_object
# 
# # get the full normalized gev parameters
# results$full_normalized_gev_parameters_object
# 
# # get gev models
# models<- results$gev_models_object
# 
# models
# 
# names(models)
# 
# # get the first model
# 
# model_1 <- models[[1]]
# 
# class(model_1)
# names(model_1)
