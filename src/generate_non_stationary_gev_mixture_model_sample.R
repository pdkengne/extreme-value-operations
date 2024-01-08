# library(dplyr)

source("./src/get_several_ns_gev_model_normalized_parameters.R")
source("./src/generate_gev_mixture_model_sample.R")


generate_non_stationary_gev_mixture_model_sample <- function(ns_gev_mixture_model_object,
                                                             n = 1,
                                                             data = NULL,
                                                             kind = c("geometric", "arithmetic", "harmonic")[1]){
  # ns_gev_mixture_model_object: an object associated with a result of the function "fit_non_stationary_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic" or "harmonic"
  # n: number of observations to generate
  # data: dataframe of covariates for linear modeling of the gev model parameters
  
  # extract the selected full non-stationary gev models 
  selected_full_ns_gev_models <- ns_gev_mixture_model_object$selected_full_ns_gev_models
  
  # extract the vector of weights
  weights <- ns_gev_mixture_model_object$weights
  
  # extract the largest data of observations
  partial_data <- ns_gev_mixture_model_object$partial_data
  
  # extract the dataset of covariates associated with the largest dataset
  partial_data_covariates <- ns_gev_mixture_model_object$partial_data_covariates
  
  # extract the threshold
  threshold <- ns_gev_mixture_model_object$threshold
  
  # extract the dataset of covariates to use
  if (is.null(data)){
    index <- which.max(partial_data)
    data <- dplyr::slice(partial_data_covariates, index)
  }
  
  # calculate the normalized gev parameters
  if (ns_gev_mixture_model_object$use_extremal_index){
    normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                data = data,
                                                                                use_extremal_index = TRUE,
                                                                                normalize_parameters = TRUE)
  } 
  else{
    normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                data = data,
                                                                                use_extremal_index = FALSE,
                                                                                normalize_parameters = TRUE)
  }
  
  # calculate the vector of cdf
  mixture_distributions <- sapply(1:nrow(data), function(i){
    distributions <- sapply(1:length(weights), function(k){
      parameters <- normalized_gev_parameters[[k]]
      
      coefficients <- c("location" = parameters$location[i], 
                        "scale" = parameters$scale[i], 
                        "shape" = parameters$shape[i])
      
      coefficients
    })
    
    sample <- generate_gev_mixture_model_sample(n = n,
                                                locations = distributions["location", ],
                                                scales = distributions["scale", ],
                                                shapes = distributions["shape", ],
                                                weights = weights,
                                                kind = kind)
    
    sample
  })
  
  output <- data.frame(mixture_distributions)
  names(output) <- paste0("covariates_", rownames(data), sep = "")
  
  output
}



# # example 1
# 
# source("./src/fit_non_stationary_gev_mixture_model.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 10000
# 
# x <- rnorm(n = n)
# 
# #x <- rexp(n = n, rate = 1)
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.01)
# 
# ns_gev_mixture_model_object <- fit_non_stationary_gev_mixture_model(x = x,
#                                                                     data = NULL,
#                                                                     nlargest = 3000,
#                                                                     block_sizes = NULL,
#                                                                     minimum_nblocks = 50,
#                                                                     threshold = NULL,
#                                                                     confidence_level = 0.95,
#                                                                     use_extremal_index = TRUE,
#                                                                     use_uniform_prior = TRUE,
#                                                                     method = c("MLE", "GMLE")[1])
# 
# range(x)
# 
# data <- dplyr::slice(ns_gev_mixture_model_object$all_data_covariates, 1:2)
# 
# results_geometric <- generate_non_stationary_gev_mixture_model_sample(ns_gev_mixture_model_object,
#                                                                       n = 1000,
#                                                                       data = NULL,
#                                                                       kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# 
# hist(results_geometric$covariates_1)
# 
# results_geometric <- generate_non_stationary_gev_mixture_model_sample(ns_gev_mixture_model_object,
#                                                                       n = 2,
#                                                                       data = data,
#                                                                       kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# results_geometric
# 
# results_arithmetic <- generate_non_stationary_gev_mixture_model_sample(ns_gev_mixture_model_object,
#                                                                        n = 10,
#                                                                        data = NULL,
#                                                                        kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# results_arithmetic
# 
# 
# results_arithmetic <- generate_non_stationary_gev_mixture_model_sample(ns_gev_mixture_model_object,
#                                                                        n = 10,
#                                                                        data = data,
#                                                                        kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# results_arithmetic
# 
# 
# results_harmonic <- generate_non_stationary_gev_mixture_model_sample(ns_gev_mixture_model_object,
#                                                                        n = 1000,
#                                                                        data = NULL,
#                                                                        kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# hist(results_harmonic$covariates_1)



