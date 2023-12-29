source("./src/calculate_gev_mixture_model_cdf.R")
source("./src/get_several_ns_gev_model_normalized_parameters.R")


calculate_non_stationary_gev_mixture_model_cdf <- function(ns_gev_mixture_model_object,
                                                           q = NULL,
                                                           data = NULL,
                                                           kind = c("geometric", "arithmetic")[1]){
  # ns_gev_mixture_model_object: an object associated with a result of the function "fit_non_stationary_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  # q: vector of observations
  # data: dataframe of covariates for linear modeling of the gev model parameters
  #       note that the following condition must be satisfied nrow(data) == length(q) or nrow(data) == 1
  
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
    y <- 1:length(partial_data)
    index <- y[partial_data > threshold]
    data <- partial_data_covariates %>% slice(index)
  }
  
  # extract the vector of observations to use
  if (is.null(q)){
    q <- partial_data[partial_data > threshold]
  }
  
  # check if the required condition on input arguments is satisfied
  if (nrow(data) != length(q) & nrow(data) > 1){
    stop("Sorry, the information in the input arguments must satisfy nrow(data) == length(q) or nrow(data) == 1 !")
  }
  
  # calculate the normalized gev parameters
  if (ns_gev_mixture_model_object$use_extremal_index){
    normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                data = data,
                                                                                use_extremal_index = TRUE)
  } 
  else{
    normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                data = data,
                                                                                use_extremal_index = FALSE)
  }
  
  # calculate the vector of cdf
  mixture_distributions <- sapply(1:nrow(data), function(i){
    obs <- x[i]
    
    distributions <- sapply(1:length(weights), function(k){
      parameters <- normalized_gev_parameters[[k]]
      
      coefficients <- c("location" = parameters$location[i], 
                        "scale" = parameters$scale[i], 
                        "shape" = parameters$shape[i])
      
      coefficients
    })
    
    cdf <- calculate_gev_mixture_model_cdf(q = obs,
                                           locations = distributions["location", ],
                                           scales = distributions["scale", ],
                                           shapes = distributions["shape", ],
                                           weights = weights,
                                           kind = kind)
    
    cdf
  })
  
  mixture_distributions
}



# # example 1
# 
# source("./src/fit_non_stationary_gev_mixture_model.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 3000
# 
# x <- rnorm(n = n)
# 
# #x <- rexp(n = n, rate = 1)
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.01)
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.01)
# 
# ns_gev_mixture_model_object <- fit_non_stationary_gev_mixture_model(x = x,
#                                                                     data = NULL,
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
# q <- median(x)
# data <- ns_gev_mixture_model_object$all_data_covariates %>% slice(1)
# 
# results_geometric <- calculate_non_stationary_gev_mixture_model_cdf(ns_gev_mixture_model_object,
#                                                                     q = NULL,
#                                                                     data = NULL,
#                                                                     kind = c("geometric", "arithmetic")[1])
# 
# results_geometric
# 
# results_geometric <- calculate_non_stationary_gev_mixture_model_cdf(ns_gev_mixture_model_object,
#                                                                     q = q,
#                                                                     data = data,
#                                                                     kind = c("geometric", "arithmetic")[1])
# 
# results_geometric
# 
# results_arithmetic <- calculate_non_stationary_gev_mixture_model_cdf(ns_gev_mixture_model_object,
#                                                                      q = NULL,
#                                                                      data = NULL,
#                                                                      kind = c("geometric", "arithmetic")[2])
# 
# results_arithmetic
# 
# results_arithmetic <- calculate_non_stationary_gev_mixture_model_cdf(ns_gev_mixture_model_object,
#                                                                      q = 3:4,
#                                                                      data = data,
#                                                                      kind = c("geometric", "arithmetic")[2])
# 
# results_arithmetic
