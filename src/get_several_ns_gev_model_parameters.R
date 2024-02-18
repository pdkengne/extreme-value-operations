source("./src/get_ns_gev_model_normalized_parameters.R")

get_several_ns_gev_model_parameters <- function(several_ns_gev_models, 
                                                data = NULL){
  # several_ns_gev_models: an object associated with a result of the function "estimate_ns_evd_cluster_models()"
  # data: dataframe of covariates for linear modeling of the gev model parameters
  
  # calculate the gev model normalized parameters object associated with every model
  nclusters <- length(several_ns_gev_models)
  
  several_gev_normalized_parameters_object <- lapply(1:nclusters, function(k){
    # extract the non-stationary gev model
    ns_gev_model <- several_ns_gev_models[[k]]
    
    # extract the dataset of covariates
    if (is.null(data)){
      data <- ns_gev_model$cov.data
    }
    
    # extract the block size and extremal index
    block_size <- 1
    extremal_index <- 1
    
    # calculate the gev model normalized parameters object
    get_ns_gev_model_normalized_parameters(ns_gev_model = ns_gev_model, 
                                           data = data,
                                           block_size = block_size,
                                           extremal_index = extremal_index)
    
  })
  
  
  several_gev_normalized_parameters_object
}


# # example 1
# 
# source("./src/estimate_ns_evd_cluster_models.R")
# source("./src/generate_gev_sample.R")
# source("./src/initialize_ns_cluster_data.R")
# 
# n <- 1000
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(0.6, 1.3), sd = c(0.1, 0.1))
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# covariates <- data.frame(trend = trend, random = rnd)
# 
# hist(x, nclass = NULL)
# 
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_ns_cluster_data(x = x,
#                                                    covariates = covariates,
#                                                    nclusters = nclusters)
# 
# names(initial_cluster_data)
# 
# initial_cluster_data$cluster_data
# 
# initial_cluster_data$cluster_covariates
# 
# 
# cluster_models <- estimate_ns_evd_cluster_models(input_cluster_data = initial_cluster_data,
#                                                  location.fun = ~ trend + random,
#                                                  scale.fun = ~ 1,
#                                                  shape.fun = ~1)
# 
# cluster_models
# 
# 
# results <- get_several_ns_gev_model_parameters(several_ns_gev_models = cluster_models,
#                                                data = NULL)
# 
# #results
# length(results)
# 
# results[[1]]
# 
# 
# results <- get_several_ns_gev_model_parameters(several_ns_gev_models = cluster_models,
#                                                data = covariates)
# 
# #results
# length(results)
# 
# results[[1]]

