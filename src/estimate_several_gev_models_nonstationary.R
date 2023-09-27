source("./src/estimate_single_gev_model.R")
source("./src/estimate_single_gev_model_nonstationary.R")

estimate_several_gev_models_nonstationary <- function(x, 
                                                      data = NULL,  
                                                      block_sizes,
                                                      threshold = NULL, 
                                                      threshold.fun = ~1, 
                                                      location.fun = ~1,
                                                      scale.fun = ~1, 
                                                      shape.fun = ~1, 
                                                      use.phi = FALSE,
                                                      type = c("GEV", "GP", "PP", "Gumbel", "Exponential")[1],
                                                      method = c("MLE", "GMLE", "Bayesian", "Lmoments")[1]){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # data: dataframe of covariates for linear modeling of the location parameter
  # threshold.fun, location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                                    must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter or not
  # type: type of model to use
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  # estimate the gev model associated with each block size
  models <- lapply(block_sizes, 
                   function(block_size)
                     estimate_single_gev_model_nonstationary(x = x, 
                                                             block_size = block_size,
                                                             data = data,
                                                             threshold = threshold, 
                                                             threshold.fun = threshold.fun, 
                                                             location.fun = location.fun,
                                                             scale.fun = scale.fun, 
                                                             shape.fun = shape.fun, 
                                                             use.phi = use.phi,
                                                             type = type,
                                                             method = method)) 
  
  # extract the extremal indexes
  extremal_indexes <- sapply(models, function(model) model$extremal_index)
  names(extremal_indexes) <- block_sizes
  
  # extract the normalized gev parameters
  normalized_gev_parameters_object <- sapply(models, function(model) model$normalized_gev_parameters)
  normalized_gev_parameters_object <- data.frame(t(normalized_gev_parameters_object))
  rownames(normalized_gev_parameters_object) <- block_sizes
  
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
  output[["covariates"]] <- data
  output[["block_maxima_object"]] <- block_maxima_object
  output[["block_maxima_indexes_object"]] <- block_maxima_indexes_object
  output[["gev_models_object"]] <- gev_models_object
  output[["block_sizes"]] <- block_sizes
  output[["extremal_indexes"]] <- extremal_indexes
  output[["normalized_gev_parameters_object"]] <- normalized_gev_parameters_object
  
  output
}



# # example 1
# 
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
# 
# results <- estimate_several_gev_models_nonstationary(x, block_sizes = equivalent_block_sizes)
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
# # get the normalized gev parameters
# results$normalized_gev_parameters_object
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
# 
# # get the the associated block maxima
# results$block_maxima_object[[1]]
# 
# # get the associated block maxima indexes
# results$block_maxima_indexes_object[[1]]
