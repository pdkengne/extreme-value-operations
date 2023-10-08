source("./src/estimate_single_ns_gev_model.R")

estimate_several_ns_gev_models <- function(x, 
                                           block_sizes, 
                                           data = NULL, 
                                           location.fun = ~1,
                                           scale.fun = ~1, 
                                           shape.fun = ~1, 
                                           use.phi = TRUE,
                                           type = c("GEV", "Gumbel")[1],
                                           method = c("MLE", "GMLE")[1]){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # data: dataframe of covariates for linear modeling of the location parameter
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # type: type of model to use
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  # estimate the gev model associated with each block size
  models <- lapply(block_sizes, 
                   function(block_size)
                     estimate_single_ns_gev_model(x = x, 
                                                  data = data,
                                                  block_size = block_size, 
                                                  location.fun = location.fun,
                                                  scale.fun = scale.fun, 
                                                  shape.fun = shape.fun, 
                                                  use.phi = use.phi,
                                                  type = type,
                                                  method = method)) 
  
  # extract the extremal indexes
  extremal_indexes <- sapply(models, function(model) model$extremal_index)
  names(extremal_indexes) <- block_sizes
  
  # extract the block maxima 
  block_maxima_object <- lapply(models, function(model) model$block_maxima)
  names(block_maxima_object) <- block_sizes
  
  # extract the block maxima indexes
  block_maxima_indexes_object <- lapply(models, function(model) model$block_maxima_indexes)
  names(block_maxima_indexes_object) <- block_sizes
  
  # extract covariates associated to block maxima
  block_maxima_covariates_object <- lapply(models, function(model) model$block_maxima_covariates)
  names(block_maxima_covariates_object) <- block_sizes
  
  # extract the gev models
  gev_models_object <- lapply(models, function(model) model$gev_model)
  names(gev_models_object) <- block_sizes
  
  # update the output object
  output[["data"]] <- x
  output[["covariates"]] <- models[[1]]$covariates
  output[["block_sizes"]] <- block_sizes
  output[["block_maxima_object"]] <- block_maxima_object
  output[["block_maxima_indexes_object"]] <- block_maxima_indexes_object
  output[["block_maxima_covariates_object"]] <- block_maxima_covariates_object
  output[["extremal_indexes"]] <- extremal_indexes
  output[["gev_models_object"]] <- gev_models_object
  
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
# n <- 10000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
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
# results <- estimate_several_ns_gev_models(x = x,
#                                           block_sizes = equivalent_block_sizes,
#                                           data = data,
#                                           location.fun = ~ .,
#                                           scale.fun = ~ .,
#                                           shape.fun = ~ 1,
#                                           use.phi = TRUE,
#                                           type = c("GEV", "Gumbel")[1],
#                                           method = c("MLE", "GMLE")[1])
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
# # get the block maxima covariates
# results$block_maxima_covariates_object
# 
# # get the full covariates
# head(results$covariates)
# tail(results$covariates)
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
