# library(extRemes)

source("./src/get_ns_gev_model_parameters.R")


get_ns_gev_mixture_model_cdf <- function(ns_gev_mixture_model){
  # ns_gev_mixture_model: an object associated with a result of the function "fit_ns_gev_mixture_model()"
  
  p <- ns_gev_mixture_model$nclusters
  
  x <- ns_gev_mixture_model$data
  
  n <- length(x)
  
  covariates <- ns_gev_mixture_model$covariates
  
  cluster_weights <- ns_gev_mixture_model$cluster_weights
  
  cluster_models <- ns_gev_mixture_model$cluster_models
  
  cluster_parameters <- lapply(cluster_models, function(model){
    parameters <- get_ns_gev_model_parameters(model, covariates)
    parameters
  })
  
  mixture_distributions <- sapply(1:n, function(i){
    obs <- x[i]
    
    distributions <- sapply(1:p, function(k){
      parameters_list <- cluster_parameters[[k]]
      
      locations <- parameters_list$location
      scales <- parameters_list$scale
      shapes <- parameters_list$shape
      
      location <- locations[i]
      scale <- scales[i]
      shape <- shapes[i]
      
      cdf <- extRemes::pevd(q = obs, 
                            loc = location, 
                            scale = scale, 
                            shape = shape, 
                            log.p = FALSE, 
                            type = "GEV")
      
      cdf
    })
    
    sum(cluster_weights*distributions)
  })
  
  mixture_distributions
}



# # example 1
# 
# source("./src/fit_ns_gev_mixture_model.R")
# source("./src/calculate_modes.R")
# source("./src/plot_modes.R")
# 
# data(faithful, package = "datasets")
# 
# data <- faithful
# 
# data$scaled_waiting <- scale(data$waiting)
# 
# names(data)
# 
# x <- data$eruptions
# 
# modes_object <- calculate_modes(x = x)
# 
# plot_modes(modes_object)
# 
# p <- 2
# 
# ns_gev_mixture_model <- fit_ns_gev_mixture_model(x = x,
#                                     data = data,
#                                     location.fun = ~ scaled_waiting,
#                                     scale.fun = ~ 1,
#                                     shape.fun = ~ 1,
#                                     use.phi = FALSE,
#                                     nb_gev_models = p,
#                                     min_cluster_size = 20,
#                                     max_iteration = 50,
#                                     tolerance = 10^(-3),
#                                     left_cluster_extension_size = 5,
#                                     right_cluster_extension_size = 10)
# 
# ns_gev_mixture_model$cluster_gev_model_coefficients
# 
# results <- get_ns_gev_mixture_model_cdf(ns_gev_mixture_model)
# 
# results
# 
# hist(results)
# 
# ks.test(x = results ,y = "punif", min = 0, max = 1)

