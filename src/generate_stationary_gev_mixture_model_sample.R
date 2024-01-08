source("./src/generate_gev_mixture_model_sample.R")


generate_stationary_gev_mixture_model_sample <- function(gev_mixture_model_object,
                                                         n = 1,
                                                         kind = c("geometric", "arithmetic", "harmonic")[1]){
  # gev_mixture_model_object: an object associated with a result of the function "fit_stationary_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic" or "harmonic"
  # n: number of observations to generate
  
  # get the normalized gev parameters
  if (gev_mixture_model_object$use_extremal_index){
    normalized_gev_parameters <- gev_mixture_model_object$full_normalized_gev_parameters_object
  } 
  else{
    normalized_gev_parameters <- gev_mixture_model_object$normalized_gev_parameters_object
  }
  
  shapes <- normalized_gev_parameters$shape_star
  scales <- normalized_gev_parameters$scale_star
  locations <- normalized_gev_parameters$loc_star
  
  # get the vector of weights
  weights <- gev_mixture_model_object$weights
  
  # simulate a vector of random values
  output <- generate_gev_mixture_model_sample(n = n, 
                                              locations = locations, 
                                              scales = scales, 
                                              shapes = shapes, 
                                              weights = weights,
                                              kind = kind)
  
  output
}



# # example 1
# 
# source("./src/fit_stationary_gev_mixture_model.R")
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
# gev_mixture_model_object <- fit_stationary_gev_mixture_model(x = x,
#                                                              block_sizes = NULL,
#                                                              minimum_nblocks = 50,
#                                                              threshold = NULL,
#                                                              confidence_level = 0.95,
#                                                              use_extremal_index = TRUE,
#                                                              use_uniform_prior = TRUE,
#                                                              method = c("MLE", "GMLE", "Lmoments")[1])
# 
# gev_mixture_model_object$unnormalized_gev_parameters_object
# gev_mixture_model_object$weights
# gev_mixture_model_object$threshold
# 
# results_geometric <- generate_stationary_gev_mixture_model_sample(gev_mixture_model_object,
#                                                                   kind = c("geometric", "arithmetic", "harmonic")[1],
#                                                                   n = n)
# 
# hist(results_geometric)
# 
# results_arithmetic <- generate_stationary_gev_mixture_model_sample(gev_mixture_model_object,
#                                                                    kind = c("geometric", "arithmetic", "harmonic")[2],
#                                                                    n = n)
# 
# hist(results_arithmetic)
# 
# results_harmonic <- generate_stationary_gev_mixture_model_sample(gev_mixture_model_object,
#                                                                    kind = c("geometric", "arithmetic", "harmonic")[3],
#                                                                    n = n)
# 
# hist(results_harmonic)
