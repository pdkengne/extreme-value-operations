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
source("./src/estimate_weibull_cluster_models.R")
source("./src/calculate_weibull_cluster_attractors.R")
source("./src/calculate_weibull_mixture_model_cdf.R")
source("./src/calculate_weibull_mixture_model_pdf.R")
source("./src/calculate_weibull_mixture_model_inverse_cdf.R")


fit_stationary_weibull_mixture_model <- function(x, 
                                                 nclusters = NULL, 
                                                 centers = NULL, 
                                                 minimum_cluster_size = 20,
                                                 prior_cluster_weights = NULL,
                                                 confidence_level = 0.95){
  # x:
  # nclusters:
  # centers:
  # minimum_cluster_size:
  # prior_cluster_weights:
  # confidence_level:
  
  initial_cluster_data <- initialize_cluster_data(x = x, 
                                                  nclusters = nclusters,
                                                  centers = centers)
  
  cluster_models <- estimate_weibull_cluster_models(cluster_data = initial_cluster_data)
  
  nclusters <- length(cluster_models)
  
  cluster_attractors <- calculate_weibull_cluster_attractors(x = x, 
                                                            cluster_models = cluster_models, 
                                                            minimum_cluster_size = minimum_cluster_size,
                                                            prior_cluster_weights = prior_cluster_weights,
                                                            confidence_level = confidence_level)
  
  previous_cluster_models_parameters <- cluster_attractors$cluster_models_coefficients
  
  cluster_models_parameters_variation <- previous_cluster_models_parameters
  
  condition <- all(apply(cluster_models_parameters_variation, 2, function(x) x == 0))
  
  iteration <- 1
  
  while (!condition){
    cluster_data_list <- cluster_attractors$cluster_data_list
    
    cluster_attractors_weights <- cluster_attractors$cluster_attractors_weights
    
    cluster_models <- estimate_weibull_cluster_models(cluster_data = cluster_data_list)
    
    cluster_attractors <- calculate_weibull_cluster_attractors(x = x, 
                                                              cluster_models = cluster_models, 
                                                              minimum_cluster_size = minimum_cluster_size,
                                                              prior_cluster_weights = cluster_attractors_weights,
                                                              confidence_level = confidence_level)
    
    selected_cluster_id <- cluster_attractors$selected_cluster_id
    
    previous_cluster_models_parameters <- previous_cluster_models_parameters[selected_cluster_id, ]
    
    current_cluster_models_parameters <- cluster_attractors$cluster_models_coefficients
    
    cluster_models_parameters_variation <- previous_cluster_models_parameters - current_cluster_models_parameters
    
    condition <- all(apply(cluster_models_parameters_variation, 2, function(x) x == 0))
    
    if (!condition){
      iteration <- iteration + 1
    }
    
    previous_cluster_models_parameters <- current_cluster_models_parameters
    
  }
  
  output <- list()
  
  output[["x"]] <- x
  output[["cluster_data_list"]] <- cluster_attractors$cluster_data_list
  output[["cluster_models"]] <- cluster_attractors$cluster_models
  output[["cluster_models_coefficients_ci"]] <- cluster_attractors$cluster_models_coefficients_ci
  output[["iteration"]] <- iteration
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
# library(mixR)
# 
# set.seed(102)
# x = mixR::rmixweibull(1000, c(0.3, 0.7), c(0.6, 1.3), c(0.1, 0.1))
# 
# mod1 = mixfit(x, ncomp = 2, family = 'weibull')
# mod1
# 
# results <- fit_stationary_weibull_mixture_model(x = x,
#                                                 nclusters = 2,
#                                                 centers = NULL,
#                                                 minimum_cluster_size = 20,
#                                                 prior_cluster_weights = NULL,
#                                                 confidence_level = 0.95)
# 
# names(results)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# results
# 
# 
# # example 2
# 
# library(mixtools)
# library(mixR)
# 
# data(faithful)
# 
# x <- faithful$waiting
# 
# x <- faithful$eruptions
# 
# mod1 = mixfit(x, ncomp = 2, family = 'weibull')
# mod1
# 
# results <- fit_stationary_weibull_mixture_model(x = x,
#                                                 nclusters = 2,
#                                                 centers = NULL,
#                                                 minimum_cluster_size = 20,
#                                                 prior_cluster_weights = NULL,
#                                                 confidence_level = 0.95)
# 
# names(results)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# results
# 
# 
# # example 3
# 
# x <- mixR::rmixweibull(n = 2000, pi = c(2/4, 1/4, 1/4), mu = c(0.6, 1.3, 2.6), sd = c(0.1, 0.1, 0.1))
# 
# mod1 = mixfit(x, ncomp = 3, family = 'weibull')
# mod1
# 
# results <- fit_stationary_weibull_mixture_model(x = x,
#                                                 nclusters = 3,
#                                                 centers = NULL,
#                                                 minimum_cluster_size = 20,
#                                                 prior_cluster_weights = NULL,
#                                                 confidence_level = 0.95)
# 
# names(results)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# results

