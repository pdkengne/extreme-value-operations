# library(dplyr)

source("./src/calculate_gev_mixture_model_inverse_cdf.R")


calculate_non_stationary_gev_mixture_model_inverse_cdf <- function(ns_gev_mixture_model_object,
                                                                   p = NULL,
                                                                   data = NULL,
                                                                   kind = c("geometric", "arithmetic", "harmonic")[1],
                                                                   iterations = 100){
  # ns_gev_mixture_model_object: an object associated with a result of the function "fit_non_stationary_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic" or "harmonic"
  # p: vector of probabilities
  # data: dataframe of covariates for linear modeling of the gev model parameters
  #       note that the following condition must be satisfied nrow(data) == length(p) or nrow(data) == 1
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
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
    index <- which(partial_data > threshold)
    data <- dplyr::slice(partial_data_covariates, index)
  }
  
  # extract the vector of probabilities to use
  if (is.null(p)){
    values <- exp(partial_data[partial_data > threshold])
    p <- order(values)/(length(values) + 1)
  }
  
  # check if the required condition on input arguments is satisfied
  if (nrow(data) != length(p) & nrow(data) > 1){
    stop("Sorry, the information in the input arguments must satisfy nrow(data) == length(p) or nrow(data) == 1 !")
  }
  
  # balance nrow(data) and length(p)
  if (nrow(data) == 1){
    idx <- rep(x = 1, times = length(p))
    data <- dplyr::slice(data, idx)
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
    prob <- p[i]
    
    distributions <- sapply(1:length(weights), function(k){
      parameters <- normalized_gev_parameters[[k]]
      
      coefficients <- c("location" = parameters$location[i], 
                        "scale" = parameters$scale[i], 
                        "shape" = parameters$shape[i])
      
      coefficients
    })
    
    inverse_cdf <- calculate_gev_mixture_model_inverse_cdf(p = prob,
                                                           locations = distributions["location", ],
                                                           scales = distributions["scale", ],
                                                           shapes = distributions["shape", ],
                                                           weights = weights,
                                                           kind = kind,
                                                           iterations = iterations)
    
    inverse_cdf
  })
  
  mixture_distributions
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
# data <- dplyr::slice(ns_gev_mixture_model_object$all_data_covariates, 1)
# 
# results_geometric <- calculate_non_stationary_gev_mixture_model_inverse_cdf(ns_gev_mixture_model_object,
#                                                                             p = NULL,
#                                                                             data = NULL,
#                                                                             kind = c("geometric", "arithmetic", "harmonic")[1],
#                                                                             iterations = 25)
# 
# hist(results_geometric, freq = FALSE)
# 
# results_geometric <- calculate_non_stationary_gev_mixture_model_inverse_cdf(ns_gev_mixture_model_object,
#                                                                             p = 0.5,
#                                                                             data = data,
#                                                                             kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# results_geometric
# 
# results_arithmetic <- calculate_non_stationary_gev_mixture_model_inverse_cdf(ns_gev_mixture_model_object,
#                                                                              p = NULL,
#                                                                              data = NULL,
#                                                                              kind = c("geometric", "arithmetic", "harmonic")[2],
#                                                                              iterations = 25)
# 
# hist(results_arithmetic, freq = FALSE)
# 
# results_arithmetic <- calculate_non_stationary_gev_mixture_model_inverse_cdf(ns_gev_mixture_model_object,
#                                                                              p = c(0.25, 0.5, 0.75),
#                                                                              data = data,
#                                                                              kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# results_arithmetic
# 
# 
# results_arithmetic <- calculate_non_stationary_gev_mixture_model_inverse_cdf(ns_gev_mixture_model_object,
#                                                                              p = c(0.99, 0.99, 0.99),
#                                                                              data = data,
#                                                                              kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# results_arithmetic
# 
# 
# results_harmonic <- calculate_non_stationary_gev_mixture_model_inverse_cdf(ns_gev_mixture_model_object,
#                                                                              p = NULL,
#                                                                              data = NULL,
#                                                                              kind = c("geometric", "arithmetic", "harmonic")[3],
#                                                                              iterations = 25)
# 
# hist(results_harmonic, freq = FALSE)
# 
# results_harmonic <- calculate_non_stationary_gev_mixture_model_inverse_cdf(ns_gev_mixture_model_object,
#                                                                              p = c(0.25, 0.5, 0.75),
#                                                                              data = data,
#                                                                              kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# results_harmonic
