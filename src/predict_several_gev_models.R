source("./src/predict_single_gev_model.R")


predict_several_gev_models <- function(several_ns_gev_models, 
                                       covariates = NULL,
                                       method = c("MLE", "GMLE", "Lmoments")[1]){
  # single_ns_gev_model: an object associated with a result of the function "estimate_several_ns_gev_models()"
  # covariates: a named list whose names match the fitted model parameter names
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  data <- several_ns_gev_models$data
  block_sizes <- several_ns_gev_models$block_sizes
  
  ns_gev_models
  
  ns_gev_models <- several_ns_gev_models$gev_models_object
  
  # estimate the gev model associated with each block size
  models <- lapply(ns_gev_models, 
                   function(single_ns_gev_model)
                     predict_single_gev_model(single_ns_gev_model = single_ns_gev_model, 
                                              covariates = covariates, 
                                              method = method)) 
  
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



# example 1

source("./src/find_minimum_block_size.R")
source("./src/find_block_size_associated_with_given_number_of_blocks.R")
source("./src/generate_gev_sample.R")
source("./src/estimate_several_ns_gev_models.R")
source("./src/plot_several_ns_standardized_block_maxima_mean.R")
source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
source("./src/plot_several_standardized_block_maxima_mean.R")
source("./src/estimate_several_standardized_block_maxima_mean.R")

x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = -0.2)

minimum_block_size <- find_minimum_block_size(x)
minimum_block_size

maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
maximum_block_size

block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
block_sizes

plot_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, equivalent = FALSE)
plot_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, equivalent = TRUE)

equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))

several_ns_gev_models <- estimate_several_ns_gev_models(x = x,
                                                        block_sizes = equivalent_block_sizes,
                                                        data = NULL,
                                                        location.fun = ~ 1,
                                                        scale.fun = ~ 1,
                                                        shape.fun = ~ 1,
                                                        use.phi = TRUE,
                                                        type = c("GEV", "Gumbel")[1],
                                                        method = c("MLE", "GMLE")[2])


# several_ns_gev_models
names(several_ns_gev_models)

models_object <- several_ns_gev_models$gev_models_object

model <- models_object[[1]]

names(model)

model$const.loc

results <- predict_several_gev_models(several_ns_gev_models, 
                                      covariates = NULL,
                                      method = c("MLE", "GMLE", "Lmoments")[1])

#results
names(results)

# get the block sizes
results$block_sizes

# get the extremal indexes
results$extremal_indexes

# get the associated block maxima
results$block_maxima_object

# get the associated block maxima indexes
results$block_maxima_indexes_object

# get the normalized gev parameters
results$normalized_gev_parameters_object

# get the full normalized gev parameters
results$full_normalized_gev_parameters_object

# get gev models
models<- results$gev_models_object

models

names(models)

# get the first model

model_1 <- models[[1]]

class(model_1)
names(model_1)
