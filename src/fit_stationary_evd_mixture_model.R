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
source("./src/estimate_evd_cluster_models.R")
source("./src/calculate_evd_cluster_attractors.R")
source("./src/calculate_gev_mixture_model_cdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")


fit_stationary_evd_mixture_model <- function(x, 
                                             nclusters = NULL, 
                                             centers = NULL, 
                                             minimum_cluster_size = 20,
                                             iteration_max = 500,
                                             prior_cluster_weights = NULL,
                                             do.ci = FALSE,
                                             confidence_level = 0.95){
  # x:
  # nclusters:
  # centers:
  # minimum_cluster_size:
  # iteration_max:
  # prior_cluster_weights:
  # do.ci,
  # confidence_level:
  
  initial_cluster_data <- initialize_cluster_data(x = x, 
                                                  nclusters = nclusters,
                                                  centers = centers)
  
  cluster_models <- estimate_evd_cluster_models(cluster_data = initial_cluster_data)
  
  nclusters <- length(cluster_models)
  
  cluster_attractors <- calculate_evd_cluster_attractors(x = x, 
                                                         cluster_models = cluster_models, 
                                                         minimum_cluster_size = minimum_cluster_size,
                                                         prior_cluster_weights = prior_cluster_weights,
                                                         do.ci = do.ci,
                                                         confidence_level = confidence_level)
  
  previous_cluster_models_parameters <- cluster_attractors$cluster_models_coefficients
  
  cluster_models_parameters_variation <- previous_cluster_models_parameters
  
  condition <- all(apply(cluster_models_parameters_variation, 2, function(x) x == 0))
  
  iteration <- 1
  
  while (!condition & iteration < ceiling(iteration_max)){
    cluster_data_list <- cluster_attractors$cluster_data_list
    
    cluster_attractors_weights <- cluster_attractors$cluster_attractors_weights
    
    cluster_models <- estimate_evd_cluster_models(cluster_data = cluster_data_list)
    
    cluster_attractors <- calculate_evd_cluster_attractors(x = x, 
                                                           cluster_models = cluster_models, 
                                                           minimum_cluster_size = minimum_cluster_size,
                                                           prior_cluster_weights = cluster_attractors_weights,
                                                           do.ci = do.ci,
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
  
  output[["clusters"]] <- cluster_attractors$clusters
  output[["x"]] <- x
  output[["cluster_data_list"]] <- cluster_attractors$cluster_data_list
  output[["cluster_models"]] <- cluster_attractors$cluster_models
  output[["cluster_models_coefficients_ci"]] <- cluster_attractors$cluster_models_coefficients_ci
  output[["iteration"]] <- iteration
  output[["cluster_models_parameters_variation"]] <- cluster_models_parameters_variation
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
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(0.6, 1.3), sd = c(0.1, 0.1))
# 
# hist(x, nclass = NULL)
# 
# mod1 = mixfit(x, ncomp = 2, family = 'weibull')
# mod1
# 
# results <- fit_stationary_evd_mixture_model(x = x,
#                                             nclusters = 2,
#                                             centers = NULL,
#                                             minimum_cluster_size = 20,
#                                             prior_cluster_weights = NULL,
#                                             confidence_level = 0.95)
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
# mod1 = mixfit(x, ncomp = 2, family = 'normal')
# mod1
# 
# mod1 = mixfit(x, ncomp = 2, family = 'gamma')
# mod1
# 
# mod1 = mixfit(x, ncomp = 2, family = 'lnorm')
# mod1
# 
# mod1 = mixfit(x, ncomp = 2, family = 'weibull')
# mod1
# 
# results <- fit_stationary_evd_mixture_model(x = x,
#                                             nclusters = 2,
#                                             centers = NULL,
#                                             minimum_cluster_size = 20,
#                                             prior_cluster_weights = NULL,
#                                             confidence_level = 0.95)
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
# source("./src/generate_gev_mixture_model_sample.R")
# 
# n <- 1000
# x <- generate_gev_mixture_model_sample(n = n,
#                                        weights = c(1/2, 1/2),
#                                        locations = c(+3, +9),
#                                        scales = c(1, 1),
#                                        shapes = c(-0.01, +0.01),
#                                        kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# hist(x, nclass = 30)
# 
# mod1 = mixfit(x, ncomp = 2, family = 'lnorm')
# mod1
# 
# results <- fit_stationary_evd_mixture_model(x = x,
#                                             nclusters = 2,
#                                             centers = NULL,
#                                             minimum_cluster_size = 20,
#                                             prior_cluster_weights = NULL,
#                                             confidence_level = 0.95)
# 
# names(results)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# results

