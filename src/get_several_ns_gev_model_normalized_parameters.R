source("./src/get_ns_gev_model_normalized_parameters.R")

get_several_ns_gev_model_normalized_parameters <- function(several_ns_gev_models, 
                                                           data = NULL, 
                                                           use_extremal_index = TRUE){
  # several_ns_gev_models: an object associated with a result of the function "estimate_several_ns_gev_models()"
  # data: dataframe of covariates for linear modeling of the gev model parameters
  # use_extremal_index: a boolean which indicates whether to use the estimates extremal indexes or not
  
  # calculate the gev model normalized parameters object associated with every model
  if (is.null(data)){
    several_gev_normalized_parameters_object <- lapply(several_ns_gev_models, function(single_ns_gev_model){
      # extract the block size
      block_size <- single_ns_gev_model$block_size
      
      # extract the extremal index
      if (use_extremal_index){
        extremal_index <- single_ns_gev_model$extremal_index
      }
      else{
        extremal_index <- 1
      }
      
      # extract the non-stationary gev model
      ns_gev_model <- single_ns_gev_model$gev_model
      
      # extract the dataset of covariates
      data <- ns_gev_model$cov.data
      
      # calculate the gev model normalized parameters object
      get_ns_gev_model_normalized_parameters(ns_gev_model = ns_gev_model, 
                                             data = data,
                                             block_size = block_size,
                                             extremal_index = extremal_index)
      
    })
  }
  else{
    several_gev_normalized_parameters_object <- lapply(several_ns_gev_models, function(single_ns_gev_model){
      # extract the block size
      block_size <- single_ns_gev_model$block_size
      
      # extract the extremal index
      if (use_extremal_index){
        extremal_index <- single_ns_gev_model$extremal_index
      }
      else{
        extremal_index <- 1
      }
      
      # extract the non-stationary gev model
      ns_gev_model <- single_ns_gev_model$gev_model
      
      # calculate the gev model normalized parameters object
      get_ns_gev_model_normalized_parameters(ns_gev_model = ns_gev_model, 
                                             data = data,
                                             block_size = block_size,
                                             extremal_index = extremal_index)
      
    })
  }
  
  several_gev_normalized_parameters_object
}


# # example 1
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
# 
# n <- 3000
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
#                                                location.fun = ~ .,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# 
# plot_several_ns_standardized_block_maxima_mean(x,
#                                                block_sizes,
#                                                confidence_level = 0.95,
#                                                equivalent = TRUE,
#                                                data = data,
#                                                location.fun = ~ .,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# 
# equivalent_block_sizes_object<- estimate_several_ns_standardized_block_maxima_mean(x,
#                                                                                    block_sizes,
#                                                                                    confidence_level = 0.95,
#                                                                                    data = data,
#                                                                                    location.fun = ~ .,
#                                                                                    scale.fun = ~ .,
#                                                                                    shape.fun = ~1,
#                                                                                    use.phi = TRUE,
#                                                                                    type = c("GEV", "Gumbel")[1],
#                                                                                    method = c("MLE", "GMLE")[2])
# 
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# several_ns_gev_models <- estimate_several_ns_gev_models(x = x,
#                                                         block_sizes = equivalent_block_sizes,
#                                                         data = data,
#                                                         location.fun = ~ .,
#                                                         scale.fun = ~ .,
#                                                         shape.fun = ~ 1,
#                                                         use.phi = TRUE,
#                                                         type = c("GEV", "Gumbel")[1],
#                                                         method = c("MLE", "GMLE")[2])
# 
# 
# results <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models, data, use_extremal_index = TRUE)
# 
# #results
# names(results)
# 
# results[[1]]
# 
# 
# results <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models, data = NULL, use_extremal_index = TRUE)
# 
# #results
# names(results)
# 
# results[[31]]

