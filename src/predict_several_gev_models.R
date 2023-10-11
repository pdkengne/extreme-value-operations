source("./src/predict_single_gev_model.R")


predict_several_gev_models <- function(several_ns_gev_models, 
                                       covariates = NULL,
                                       method = c("MLE", "GMLE", "Lmoments")[1]){
  # single_ns_gev_model: an object associated with a result of the function "estimate_several_ns_gev_models()"
  # covariates: a named list whose names match the fitted model parameter names
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  # predict the gev model associated with each block size
  models <- lapply(several_ns_gev_models, 
                   function(single_ns_gev_model)
                     predict_single_gev_model(single_ns_gev_model = single_ns_gev_model, 
                                              covariates = covariates, 
                                              method = method)) 
  
  # extract the vector of observations
  x <- models[[1]]$data
  
  # extract the block sizes
  block_sizes <- sapply(models, function(model) model$block_size)
  
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
# source("./src/estimate_several_ns_gev_models.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
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
# several_ns_gev_models <- estimate_several_ns_gev_models(x = x,
#                                                         block_sizes = equivalent_block_sizes,
#                                                         data = NULL,
#                                                         location.fun = ~ 1,
#                                                         scale.fun = ~ 1,
#                                                         shape.fun = ~ 1,
#                                                         use.phi = TRUE,
#                                                         type = c("GEV", "Gumbel")[1],
#                                                         method = c("MLE", "GMLE")[1])
# 
# 
# # several_ns_gev_models
# names(several_ns_gev_models)
# 
# results <- predict_several_gev_models(several_ns_gev_models, 
#                                       covariates = NULL,
#                                       method = c("MLE", "GMLE", "Lmoments")[1])
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
# 
# 
# # example 2
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/estimate_several_ns_gev_models.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
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
# plot_several_ns_standardized_block_maxima_mean(x, 
#                                                block_sizes, 
#                                                confidence_level = 0.95, 
#                                                equivalent = FALSE,
#                                                data = data,
#                                                location.fun = ~ trend,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~ random,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[1])
# 
# 
# plot_several_ns_standardized_block_maxima_mean(x, 
#                                                block_sizes, 
#                                                confidence_level = 0.95, 
#                                                equivalent = TRUE,
#                                                data = data,
#                                                location.fun = ~ trend,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~ random,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[1])
# 
# 
# equivalent_block_sizes_object<- estimate_several_ns_standardized_block_maxima_mean(x, 
#                                                                                    block_sizes, 
#                                                                                    confidence_level = 0.95, 
#                                                                                    data = data,
#                                                                                    location.fun = ~ trend,
#                                                                                    scale.fun = ~ .,
#                                                                                    shape.fun = ~ random,
#                                                                                    use.phi = TRUE,
#                                                                                    type = c("GEV", "Gumbel")[1],
#                                                                                    method = c("MLE", "GMLE")[1])
# 
# 
# 
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# several_ns_gev_models <- estimate_several_ns_gev_models(x = x,
#                                                         block_sizes = equivalent_block_sizes,
#                                                         data = data,
#                                                         location.fun = ~ trend,
#                                                         scale.fun = ~ .,
#                                                         shape.fun = ~ random,
#                                                         use.phi = TRUE,
#                                                         type = c("GEV", "Gumbel")[1],
#                                                         method = c("MLE", "GMLE")[1])
# 
# 
# # several_ns_gev_models
# names(several_ns_gev_models)
# 
# single_ns_gev_model <- several_ns_gev_models[[1]]
# 
# ns_gev_model <- single_ns_gev_model$gev_model
# 
# ns_gev_model
# names(ns_gev_model)
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$results$par
# 
# ns_gev_model$par.models$term.names
# 
# names(data)
# 
# covariates <- list(mu1 = 25/n,
#                    phi1 = 25/n,
#                    phi2 = runif(n = 1, min = -0.5, max = 0.5),
#                    xi1 = runif(n = 1, min = -0.5, max = 0.5))
# 
# covariates
# 
# 
# results <- predict_several_gev_models(several_ns_gev_models, 
#                                       covariates = covariates,
#                                       method = c("MLE", "GMLE", "Lmoments")[1])
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
# 
# 
# # example 3
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/estimate_several_ns_gev_models.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
# source("./src/plot_several_standardized_block_maxima_mean.R")
# source("./src/estimate_several_standardized_block_maxima_mean.R")
# 
# n <- 10000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0)
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
# plot_several_ns_standardized_block_maxima_mean(x, 
#                                                block_sizes, 
#                                                confidence_level = 0.95, 
#                                                equivalent = FALSE,
#                                                data = data,
#                                                location.fun = ~ trend,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~ 1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[1])
# 
# 
# plot_several_ns_standardized_block_maxima_mean(x, 
#                                                block_sizes, 
#                                                confidence_level = 0.95, 
#                                                equivalent = TRUE,
#                                                data = data,
#                                                location.fun = ~ trend,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~ 1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[1])
# 
# 
# equivalent_block_sizes_object<- estimate_several_ns_standardized_block_maxima_mean(x, 
#                                                                                    block_sizes, 
#                                                                                    confidence_level = 0.95, 
#                                                                                    data = data,
#                                                                                    location.fun = ~ trend,
#                                                                                    scale.fun = ~ .,
#                                                                                    shape.fun = ~ 1,
#                                                                                    use.phi = TRUE,
#                                                                                    type = c("GEV", "Gumbel")[1],
#                                                                                    method = c("MLE", "GMLE")[1])
# 
# 
# 
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# several_ns_gev_models <- estimate_several_ns_gev_models(x = x,
#                                                         block_sizes = equivalent_block_sizes,
#                                                         data = data,
#                                                         location.fun = ~ trend,
#                                                         scale.fun = ~ .,
#                                                         shape.fun = ~ 1,
#                                                         use.phi = TRUE,
#                                                         type = c("GEV", "Gumbel")[1],
#                                                         method = c("MLE", "GMLE")[1])
# 
# 
# # several_ns_gev_models
# names(several_ns_gev_models)
# 
# single_ns_gev_model <- several_ns_gev_models[[1]]
# 
# ns_gev_model <- single_ns_gev_model$gev_model
# 
# ns_gev_model
# names(ns_gev_model)
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$results$par
# 
# ns_gev_model$par.models$term.names
# 
# names(data)
# 
# covariates <- list(mu0 = 1,
#                    mu1 = 25/n,
#                    phi0 = 1,
#                    phi1 = 25/n,
#                    phi2 = runif(n = 1, min = -0.5, max = 0.5))
# 
# covariates
# 
# results <- predict_single_gev_model(single_ns_gev_model = single_ns_gev_model,
#                                     covariates = NULL,
#                                     type = c("GEV", "Gumbel")[1],
#                                     method = c("MLE", "GMLE", "Lmoments")[1])
# 
# results <- predict_several_gev_models(several_ns_gev_models, 
#                                       covariates = covariates,
#                                       method = c("MLE", "GMLE", "Lmoments")[1])
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

