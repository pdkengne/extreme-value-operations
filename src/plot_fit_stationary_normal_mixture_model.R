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
source("./src/estimate_normal_cluster_models.R")
source("./src/calculate_normal_cluster_attractors.R")
source("./src/calculate_normal_mixture_model_cdf.R")
source("./src/calculate_normal_mixture_model_pdf.R")
source("./src/calculate_normal_mixture_model_inverse_cdf.R")


plot_fit_stationary_normal_mixture_model <- function(normal_mixture_model_object,
                                                     nclass = NULL,
                                                     xlab = "support",
                                                     ylab = "density",
                                                     main = "density plot",
                                                     legend_position = "topleft"){
  # normal_mixture_model_object: an object associated with a result of the function "fit_stationary_normal_mixture_model()"
  # nclass:
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  # legend_position: position of the legend
  
  x <- normal_mixture_model_object$x
  
  cluster_models_parameters <- normal_mixture_model_object$cluster_models_coefficients
  
  locations <- cluster_models_parameters[, "mean"]
  scales <- cluster_models_parameters[, "sd"]
  
  cluster_attractors_weights <- normal_mixture_model_object$cluster_attractors_weights
  
  density_object <- density(x)
  
  density_empirical <- density_object$y
  
  support_empirical <- density_object$x
  
  support <- seq(from = min(support_empirical), 
                 to = max(support_empirical), 
                 length.out = 1000)
  
  density_geometric <- calculate_normal_mixture_model_pdf(x = support, 
                                                          locations = locations, 
                                                          scales = scales, 
                                                          weights = cluster_attractors_weights,
                                                          kind = c("geometric", "arithmetic")[1])
  
  density_arithmetic <- calculate_normal_mixture_model_pdf(x = support, 
                                                           locations = locations, 
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
# source("./src/fit_stationary_normal_mixture_model.R")
# 
# library(mixR)
# 
# set.seed(102)
# x = rmixnormal(1000, c(0.3, 0.7), c(-2, 3), c(2, 1))
# 
# mod1 = mixfit(x, ncomp = 2, family = "normal")
# mod1
# 
# normal_mixture_model_object <- fit_stationary_normal_mixture_model(x = x,
#                                                                    nclusters = 2,
#                                                                    centers = NULL,
#                                                                    minimum_cluster_size = 20,
#                                                                    prior_cluster_weights = NULL,
#                                                                    confidence_level = 0.95)
# 
# names(normal_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# normal_mixture_model_object
# 
# plot_fit_stationary_normal_mixture_model(normal_mixture_model_object = normal_mixture_model_object,
#                                          nclass = NULL,
#                                          xlab = "support",
#                                          ylab = "density",
#                                          main = "density plot",
#                                          legend_position = "topleft")
# 
# 
# # example 2
# 
# source("./src/fit_stationary_normal_mixture_model.R")
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
# mod1 = mixfit(x, ncomp = 2, family = "normal")
# mod1
# 
# normal_mixture_model_object <- fit_stationary_normal_mixture_model(x = x,
#                                                                    nclusters = 2,
#                                                                    centers = NULL,
#                                                                    minimum_cluster_size = 20,
#                                                                    prior_cluster_weights = NULL,
#                                                                    confidence_level = 0.95)
# 
# names(normal_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# normal_mixture_model_object
# 
# plot_fit_stationary_normal_mixture_model(normal_mixture_model_object = normal_mixture_model_object,
#                                          nclass = NULL,
#                                          xlab = "support",
#                                          ylab = "density",
#                                          main = "density plot",
#                                          legend_position = "topleft")
# 
# 
# 
# # example 3
# 
# source("./src/fit_stationary_normal_mixture_model.R")
# 
# x <- bmixture::rmixnorm(n = 1000, weight = c(2/4, 1/4, 1/4), mean = c(-2, +2, +10), sd = c(1, 1, 1))
# 
# mod1 = mixfit(x, ncomp = 3, family = "normal")
# mod1
# 
# normal_mixture_model_object <- fit_stationary_normal_mixture_model(x = x,
#                                                                    nclusters = 3,
#                                                                    centers = NULL,
#                                                                    minimum_cluster_size = 20,
#                                                                    prior_cluster_weights = NULL,
#                                                                    confidence_level = 0.95)
# 
# names(normal_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# normal_mixture_model_object
# 
# plot_fit_stationary_normal_mixture_model(normal_mixture_model_object = normal_mixture_model_object,
#                                          nclass = NULL,
#                                          xlab = "support",
#                                          ylab = "density",
#                                          main = "density plot",
#                                          legend_position = "topleft")
# 
# 
# # example 4
# 
# source("./src/fit_stationary_unimodal_normal_mixture_model.R")
# 
# library(mixR)
# 
# x = rnorm(n = 1000)
# 
# mod1 = mixfit(x, ncomp = 1, family = "normal")
# mod1
# plot(mod1)
# 
# normal_mixture_model_object <- fit_stationary_unimodal_normal_mixture_model(x = x,
#                                                                             nclusters = 10,
#                                                                             confidence_level = 0.95)
# 
# names(normal_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# normal_mixture_model_object$cluster_attractors_frequencies
# normal_mixture_model_object$cluster_attractors_weights
# normal_mixture_model_object$cluster_models_coefficients
# normal_mixture_model_object$cluster_information_criteria
# 
# 
# plot_fit_stationary_normal_mixture_model(normal_mixture_model_object = normal_mixture_model_object,
#                                          nclass = 30,
#                                          xlab = "support",
#                                          ylab = "density",
#                                          main = "density plot",
#                                          legend_position = "topleft")
# 
# 
# # example 5
# 
# source("./src/fit_stationary_unimodal_normal_mixture_model.R")
# source("./src/generate_normal_mixture_model_sample.R")
# 
# library(mixR)
# 
# weights <- c(0.5, 0.5)
# 
# scales <- c(1, 1)
# locations <- c(-2, +2)
# 
# n <- 10000
# 
# x <- generate_normal_mixture_model_sample(n = n,
#                                           locations,
#                                           scales,
#                                           weights,
#                                           kind = c("geometric", "arithmetic")[1])
# 
# hist(x, nclass = NULL)
# 
# mod1 = mixfit(x, ncomp = 1, family = "normal")
# mod1
# plot(mod1)
# 
# normal_mixture_model_object <- fit_stationary_unimodal_normal_mixture_model(x = x,
#                                                                             nclusters = 5,
#                                                                             confidence_level = 0.95)
# 
# names(normal_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# normal_mixture_model_object$cluster_attractors_frequencies
# normal_mixture_model_object$cluster_attractors_weights
# normal_mixture_model_object$cluster_models_coefficients
# normal_mixture_model_object$cluster_information_criteria
# 
# 
# 
# plot_fit_stationary_normal_mixture_model(normal_mixture_model_object = normal_mixture_model_object,
#                                          nclass = 40,
#                                          xlab = "support",
#                                          ylab = "density",
#                                          main = "density plot",
#                                          legend_position = "topleft")
# 
# 
# # example 6
# 
# source("./src/fit_stationary_unimodal_normal_mixture_model.R")
# 
# library(mixR)
# 
# x = rgamma(n = 5000, shape = 2, rate = 3)
# 
# x <- rlogis(n = 10000, location = 0, scale = 1)
# 
# hist(x, nclass = 30)
# 
# mod1 = mixfit(x, ncomp = 1, family = "normal")
# mod1
# plot(mod1)
# 
# normal_mixture_model_object <- fit_stationary_unimodal_normal_mixture_model(x = x,
#                                                                             nclusters = 10,
#                                                                             minimum_cluster_size = 20,
#                                                                             confidence_level = 0.95)
# 
# names(normal_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# normal_mixture_model_object$cluster_attractors_frequencies
# normal_mixture_model_object$cluster_attractors_weights
# normal_mixture_model_object$cluster_models_coefficients
# normal_mixture_model_object$cluster_information_criteria
# 
# 
# 
# plot_fit_stationary_normal_mixture_model(normal_mixture_model_object = normal_mixture_model_object,
#                                          nclass = NULL,
#                                          xlab = "support",
#                                          ylab = "density",
#                                          main = "density plot",
#                                          legend_position = "topleft")
# 
# 
# # example 7
# 
# source("./src/fit_stationary_unimodal_normal_mixture_model.R")
# source("./src/generate_normal_mixture_model_sample.R")
# 
# library(mixR)
# 
# weights <- c(0.2, 0.5, 0.3)
# 
# scales <- c(1, 1, 1)
# locations <- c(-10, 0, +10)
# 
# n <- 10000
# 
# x <- generate_normal_mixture_model_sample(n = n,
#                                           locations,
#                                           scales,
#                                           weights,
#                                           kind = c("geometric", "arithmetic")[1])
# 
# hist(x, nclass = NULL)
# 
# mod1 = mixfit(x, ncomp = 1, family = "normal")
# mod1
# plot(mod1)
# 
# normal_mixture_model_object <- fit_stationary_unimodal_normal_mixture_model(x = x,
#                                                                             nclusters = 10,
#                                                                             confidence_level = 0.95)
# 
# names(normal_mixture_model_object)
# 
# # [1] "x"                              "cluster_data_list"              "cluster_models"
# # [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# # [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# # [10] "loglik"                         "cluster_information_criteria"
# 
# normal_mixture_model_object$cluster_attractors_frequencies
# normal_mixture_model_object$cluster_attractors_weights
# normal_mixture_model_object$cluster_models_coefficients
# normal_mixture_model_object$cluster_information_criteria
# 
# 
# 
# plot_fit_stationary_normal_mixture_model(normal_mixture_model_object = normal_mixture_model_object,
#                                          nclass = 30,
#                                          xlab = "support",
#                                          ylab = "density",
#                                          main = "density plot",
#                                          legend_position = "topleft")
