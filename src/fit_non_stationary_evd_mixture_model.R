# library(ggplot2)
# library(fitdistrplus)
# library(dplyr)
# library(dbscan)
# library(bmixture)


source("./src/get_knn.R")
source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/make_weights.R")
source("./src/initialize_cluster_data.R")
source("./src/initialize_ns_cluster_data.R")
source("./src/estimate_evd_cluster_models.R")
source("./src/estimate_ns_evd_cluster_models.R")
source("./src/calculate_evd_cluster_attractors.R")
source("./src/calculate_ns_evd_cluster_attractors.R")
source("./src/fit_stationary_evd_mixture_model.R")
source("./src/calculate_gev_mixture_model_cdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")


fit_non_stationary_evd_mixture_model <- function(x, 
                                                 covariates = NULL,
                                                 location.fun = ~1,
                                                 scale.fun = ~ 1, 
                                                 shape.fun = ~1, 
                                                 use.phi = TRUE,
                                                 type = c("GEV", "Gumbel")[1],
                                                 method = c("MLE", "GMLE")[1],
                                                 nclusters = NULL, 
                                                 centers = NULL, 
                                                 minimum_cluster_size = 20,
                                                 iteration_max = 500,
                                                 prior_cluster_weights = NULL,
                                                 do.ci = FALSE,
                                                 confidence_level = 0.95){
  # x:
  # covariates:
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # type: type of model to use
  # method: estimation method to use:
  # nclusters:
  # centers:
  # minimum_cluster_size:
  # iteration_max:
  # prior_cluster_weights:
  # do.ci,
  # confidence_level:
  
  evd_mixture_model_object <- fit_stationary_evd_mixture_model(x = x, 
                                                               nclusters = nclusters, 
                                                               centers = centers, 
                                                               minimum_cluster_size = minimum_cluster_size,
                                                               iteration_max = iteration_max,
                                                               prior_cluster_weights = prior_cluster_weights,
                                                               do.ci = do.ci,
                                                               confidence_level = confidence_level)
  
  
  cluster_attractors_frequencies_table <- evd_mixture_model_object$clusters
  
  cluster_attractors_weights <- evd_mixture_model_object$cluster_attractors_weights
  
  cluster_data_list <- evd_mixture_model_object$cluster_data_list
  
  nclusters <- length(cluster_data_list)
  
  if (is.null(covariates)){
    covariates <- data.frame("intercept" = rep(x = 1, times = length(x)))
  }
  
  cluster_covariates_list <- lapply(1:nclusters, function(k){
    positions <- which(cluster_attractors_frequencies_table == k)
    
    predictors <- covariates %>% slice(positions)
    
    predictors
  })
  
  input_cluster_data <- list("cluster_data" = cluster_data_list,
                             "cluster_covariates" = cluster_covariates_list)
  
  cluster_models <- estimate_ns_evd_cluster_models(input_cluster_data = input_cluster_data,
                                                   location.fun = location.fun,
                                                   scale.fun = scale.fun,
                                                   shape.fun = shape.fun,
                                                   use.phi = use.phi,
                                                   type = type,
                                                   method = method)
  
  cluster_attractors <- calculate_ns_evd_cluster_attractors(x = x,
                                                            covariates = covariates,
                                                            cluster_models = cluster_models, 
                                                            minimum_cluster_size = minimum_cluster_size,
                                                            prior_cluster_weights = cluster_attractors_weights,
                                                            do.ci = do.ci,
                                                            confidence_level = confidence_level)
                                                                  
  output <- list()
  
  output[["clusters"]] <- cluster_attractors$clusters
  output[["x"]] <- x
  output[["covariates"]] <- covariates
  output[["cluster_data_list"]] <- cluster_attractors$cluster_data_list
  output[["cluster_covariates_list"]] <- cluster_attractors$cluster_covariates_list
  output[["cluster_models"]] <- cluster_attractors$cluster_models
  output[["cluster_models_coefficients_ci"]] <- cluster_attractors$cluster_models_coefficients_ci
  output[["iteration"]] <- evd_mixture_model_object$iteration
  output[["cluster_models_parameters_variation"]] <- evd_mixture_model_object$cluster_models_parameters_variation
  output[["cluster_attractors_frequencies"]] <- cluster_attractors$cluster_attractors_frequencies
  output[["cluster_attractors_weights"]] <- cluster_attractors$cluster_attractors_weights
  output[["cluster_attractors_centers"]] <- cluster_attractors$cluster_attractors_centers
  output[["cluster_models_coefficients"]] <- cluster_attractors$cluster_models_coefficients
  output[["loglik"]] <- cluster_attractors$loglik
  output[["cluster_information_criteria"]] <- cluster_attractors$cluster_information_criteria
  
  output
  
}


# # example 1
# 
# source("./src/initialize_ns_cluster_data.R")
# source("./src/estimate_ns_evd_cluster_models.R")
# 
# n <- 1000
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(0.6, 1.3), sd = c(0.1, 0.1))
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- 1:n/n
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
# # initial_cluster_data$cluster_data
# 
# # initial_cluster_data$cluster_covariates
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
# prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))
# 
# prior_cluster_weights
# 
# 
# results <- fit_non_stationary_evd_mixture_model(x = x,
#                                                 covariates = covariates,
#                                                 location.fun = ~ trend + random,
#                                                 scale.fun = ~ 1,
#                                                 shape.fun = ~1,
#                                                 nclusters = 2,
#                                                 centers = NULL,
#                                                 minimum_cluster_size = 20,
#                                                 iteration_max = 500,
#                                                 prior_cluster_weights = NULL,
#                                                 confidence_level = 0.95)
# 
# names(results)
# 
# # [1] "clusters"                            "x"                                   "covariates"                          "cluster_data_list"                  
# # [5] "cluster_covariates_list"             "cluster_models"                      "cluster_models_parameters_variation" "cluster_attractors_frequencies"     
# # [9] "cluster_attractors_weights"          "cluster_attractors_centers"          "cluster_models_coefficients"         "loglik"                             
# # [13] "cluster_information_criteria"     
# 
# results$clusters
# 
# results$iteration
# 
# results$cluster_models
# 
# results$cluster_attractors_frequencies
# 
# results$cluster_attractors_centers
# 
# results$cluster_attractors_weights
# 
# results$cluster_models_coefficients
# 
# results$loglik
# 
# results$cluster_information_criteria
# 
# results$cluster_models_parameters_variation
