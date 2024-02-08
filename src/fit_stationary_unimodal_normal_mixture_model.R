# library(ggplot2)
# library(fitdistrplus)
# library(dplyr)
# library(dbscan)
# library(bmixture)


source("./src/get_knn.R")
source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/make_weights.R")
source("./src/extract_cluster_infos.R")
source("./src/estimate_normal_cluster_models.R")
source("./src/calculate_normal_cluster_attractors.R")
source("./src/calculate_normal_mixture_model_cdf.R")
source("./src/calculate_normal_mixture_model_pdf.R")
source("./src/calculate_normal_mixture_model_inverse_cdf.R")


fit_stationary_unimodal_normal_mixture_model <- function(x, 
                                                         nclusters = NULL,
                                                         confidence_level = 0.95){
  # x:
  # nclusters:
  # confidence_level:
  
  cluster_infos_object <- extract_cluster_infos(x = x, nclusters = nclusters)
  
  cluster_weights <- cluster_infos_object$cluster_weights
  
  cluster_sizes <- cluster_infos_object$cluster_sizes
  
  cluster_centers <- cluster_infos_object$cluster_centers
  
  cluster_data <- cluster_infos_object$cluster_data
  
  cluster_models <- estimate_normal_cluster_models(cluster_data = cluster_data)
  
  nclusters <- length(cluster_models)
  
  cluster_models_coefficients <- lapply(1:nclusters, function(k){
    model <- cluster_models[[k]]
    model$estimate
  })
  
  cluster_models_coefficients <- do.call(what = rbind, cluster_models_coefficients)
  
  cluster_models_coefficients_ci <- lapply(1:nclusters, function(k){
    model <- cluster_models[[k]]
    confint(object = model, level = confidence_level)
  })
  
  locations <- cluster_models_coefficients[, "mean"]
  scales <- cluster_models_coefficients[, "sd"]
  
  densities <- calculate_normal_mixture_model_pdf(x = x, 
                                                  locations = locations, 
                                                  scales = scales, 
                                                  weights = cluster_weights,
                                                  kind = c("geometric", "arithmetic")[1])
  
  loglik <- sum(log(densities))
  
  p <- nclusters
  q <- ncol(cluster_models_coefficients)
  n <- length(x)
  
  aic <- -2*loglik + 2*(q*p + p - 1)
  bic <- -2*loglik + log(n)*(q*p + p - 1)
  
  cluster_information_criteria <- c(aic, bic)
  names(cluster_information_criteria) <- c("AIC", "BIC")
  
  output <- list()
  
  output[["x"]] <- x
  output[["cluster_data_list"]] <- cluster_data
  output[["cluster_models"]] <- cluster_models
  output[["cluster_models_coefficients_ci"]] <- cluster_models_coefficients_ci
  output[["cluster_attractors_frequencies"]] <- cluster_sizes
  output[["cluster_attractors_weights"]] <- cluster_weights
  output[["cluster_attractors_centers"]] <- cluster_centers
  output[["cluster_models_coefficients"]] <- cluster_models_coefficients
  output[["loglik"]] <- loglik
  output[["cluster_information_criteria"]] <- cluster_information_criteria
  
  output
  
}


# # example 1
# 
# library(mixR)
# 
# x = rnorm(n = 1000)
# 
# mod1 = mixfit(x, ncomp = 1, family = "normal")
# mod1
# 
# results <- fit_stationary_unimodal_normal_mixture_model(x = x,
#                                                         nclusters = 2,
#                                                         confidence_level = 0.95)
# 
# names(results)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# results



