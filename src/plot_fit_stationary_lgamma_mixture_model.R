# library(ggplot2)
# library(fitdistrplus)
# library(dplyr)
# library(dbscan)
# library(bmixture)

# library(actuar)


source("./src/get_knn.R")
source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/make_weights.R")
source("./src/initialize_cluster_data.R")
source("./src/estimate_lgamma_cluster_models.R")
source("./src/calculate_lgamma_cluster_attractors.R")
source("./src/calculate_lgamma_mixture_model_cdf.R")
source("./src/calculate_lgamma_mixture_model_pdf.R")
source("./src/calculate_lgamma_mixture_model_inverse_cdf.R")


plot_fit_stationary_lgamma_mixture_model <- function(lgamma_mixture_model_object,
                                                     nclass = NULL,
                                                     xlab = "support",
                                                     ylab = "density",
                                                     main = "density plot",
                                                     legend_position = "topleft"){
  # lgamma_mixture_model_object: an object associated with a result of the function "fit_stationary_lgamma_mixture_model()"
  # nclass:
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  # legend_position: position of the legend
  
  x <- lgamma_mixture_model_object$x
  
  cluster_models_parameters <- lgamma_mixture_model_object$cluster_models_coefficients
  
  shapes <- cluster_models_parameters[, "shapelog"]
  scales <- cluster_models_parameters[, "ratelog"]
  
  cluster_attractors_weights <- lgamma_mixture_model_object$cluster_attractors_weights
  
  density_object <- density(x)
  
  density_empirical <- density_object$y
  
  support_empirical <- density_object$x
  
  support <- seq(from = min(support_empirical), 
                 to = max(support_empirical), 
                 length.out = 1000)
  
  density_geometric <- calculate_lgamma_mixture_model_pdf(x = support, 
                                                          shapes = shapes, 
                                                          scales = scales, 
                                                          weights = cluster_attractors_weights,
                                                          kind = c("geometric", "arithmetic")[1])
  
  density_arithmetic <- calculate_lgamma_mixture_model_pdf(x = support, 
                                                           shapes = shapes, 
                                                           scales = scales, 
                                                           weights = cluster_attractors_weights,
                                                           kind = c("geometric", "arithmetic")[2])
  
  density_range <- range(c(density_empirical, density_geometric, density_arithmetic))
  
  hist(x = x, 
       probability = TRUE,
       nclass = nclass,
       ylim = density_range, 
       xlim = range(support),
       xlab = xlab,
       ylab = ylab,
       main = main)
  
  lines(support, density_geometric, col = 6, lwd = 2)
  lines(support, density_arithmetic, col = 7, lwd = 2)
  
  legend(legend_position, 
         legend = c("theoretical_geometric", "theoretical_arithmetic"),
         lty = c(1, 1), col = c(6, 7), lwd = 2)
  
  
}


# # example 1
# 
# source("./src/fit_stationary_lgamma_mixture_model.R")
# 
# library(mixR)
# 
# n <- 2000
# 
# x <- bmixture::rmixgamma(n = n, weight = c(2/5, 3/5), alpha = c(9, 7), beta = c(0.5, 1))
# 
# hist(x, nclass = 30)
# 
# mod1 = mixfit(x, ncomp = 2, family = 'gamma')
# mod1
# 
# lgamma_mixture_model_object <- fit_stationary_lgamma_mixture_model(x = x,
#                                                                  nclusters = 2,
#                                                                  centers = NULL,
#                                                                  minimum_cluster_size = 20,
#                                                                  prior_cluster_weights = NULL,
#                                                                  confidence_level = 0.95)
# 
# names(lgamma_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# lgamma_mixture_model_object
# 
# plot_fit_stationary_lgamma_mixture_model(lgamma_mixture_model_object = lgamma_mixture_model_object,
#                                         nclass = NULL,
#                                         xlab = "support",
#                                         ylab = "density",
#                                         main = "density plot",
#                                         legend_position = "topright")
# 
# 
# # example 2
# 
# source("./src/fit_stationary_lgamma_mixture_model.R")
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
# mod1 = mixfit(x, ncomp = 2, family = 'gamma')
# mod1
# 
# lgamma_mixture_model_object <- fit_stationary_lgamma_mixture_model(x = x,
#                                                                  nclusters = 2,
#                                                                  centers = NULL,
#                                                                  minimum_cluster_size = 20,
#                                                                  prior_cluster_weights = NULL,
#                                                                  confidence_level = 0.95)
# 
# names(lgamma_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# lgamma_mixture_model_object
# 
# plot_fit_stationary_lgamma_mixture_model(lgamma_mixture_model_object = lgamma_mixture_model_object,
#                                         nclass = NULL,
#                                         xlab = "support",
#                                         ylab = "density",
#                                         main = "density plot",
#                                         legend_position = "topright")

