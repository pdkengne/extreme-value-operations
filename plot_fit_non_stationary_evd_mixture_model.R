# library(ggplot2)
# library(fitdistrplus)
# library(dplyr)
# library(dbscan)
# library(bmixture)


source("./src/get_knn.R")
source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/make_weights.R")
source("./src/initialize_ns_cluster_data.R")
source("./src/estimate_ns_evd_cluster_models.R")
source("./src/calculate_ns_evd_cluster_attractors.R")
source("./src/calculate_gev_mixture_model_cdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")
source("./src/get_several_ns_gev_model_parameters.R")


plot_fit_non_stationary_evd_mixture_model <- function(evd_mixture_model_object,
                                                      data_index = 0,
                                                      nclass = NULL,
                                                      display_all = FALSE,
                                                      xlab = "support",
                                                      ylab = "density",
                                                      main = "density plot",
                                                      legend_position = "topleft"){
  # evd_mixture_model_object: an object associated with a result of the function "fit_stationary_ns_evd_mixture_model()"
  # data_index:
  # nclass:
  # display_all:
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  # legend_position: position of the legend
  
  x <- evd_mixture_model_object$x
  covariates <- evd_mixture_model_object$covariates
  cluster_models <- evd_mixture_model_object$cluster_models
  cluster_attractors_weights <- evd_mixture_model_object$cluster_attractors_weights
  
  if (data_index > nrow(covariates) | data_index < 0){
    stop(paste0("Sorry, the value of the data_index argument must be a positive integer smaller than or equal to: ", nrow(covariates)))
  }
  
  data_index <- floor(data_index)
  
  if (data_index == 0){
    index <- which.max(x)
    data <- dplyr::slice(covariates, index)
  }
  else{
    data <- dplyr::slice(covariates, data_index)
  }
  
  cluster_models_parameters <- get_several_ns_gev_model_parameters(several_ns_gev_models = cluster_models, 
                                                                   data = data)
  
  cluster_models_parameters <- do.call(rbind, cluster_models_parameters)
  
  shapes <- cluster_models_parameters$shape
  scales <- cluster_models_parameters$scale
  locations <- cluster_models_parameters$location
  
  
  
  density_object <- density(x)
  
  density_empirical <- density_object$y
  
  support_empirical <- density_object$x
  
  support <- seq(from = min(support_empirical), 
                 to = max(support_empirical), 
                 length.out = 1000)
  
  density_arithmetic <- calculate_gev_mixture_model_pdf(x = support, 
                                                        shapes = shapes, 
                                                        scales = scales, 
                                                        locations = locations,
                                                        weights = cluster_attractors_weights,
                                                        kind = c("geometric", "arithmetic", "harmonic")[2])
  
  if (display_all){
    density_geometric <- calculate_gev_mixture_model_pdf(x = support, 
                                                         shapes = shapes, 
                                                         scales = scales, 
                                                         locations = locations,
                                                         weights = cluster_attractors_weights,
                                                         kind = c("geometric", "arithmetic", "harmonic")[1])
    
    density_harmonic <- calculate_gev_mixture_model_pdf(x = support, 
                                                        shapes = shapes, 
                                                        scales = scales, 
                                                        locations = locations,
                                                        weights = cluster_attractors_weights,
                                                        kind = c("geometric", "arithmetic", "harmonic")[3])
    
    density_range <- range(c(density_empirical, density_geometric, density_arithmetic, density_harmonic))
    
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
    lines(support, density_harmonic, col = 4, lwd = 2)
    
    legend(legend_position, 
           legend = c("theoretical_geometric", "theoretical_arithmetic", "theoretical_harmonic"),
           lty = c(1, 1, 1), col = c(6, 7, 4), lwd = 2)
  }
  else {
    density_range <- range(c(density_empirical, density_arithmetic))
    
    hist(x = x, 
         probability = TRUE,
         nclass = nclass,
         ylim = density_range, 
         xlim = range(support),
         xlab = xlab,
         ylab = ylab,
         main = main)
    
    lines(support, density_arithmetic, col = 7, lwd = 2)
    
    legend(legend_position, 
           legend = c("theoretical_arithmetic"),
           lty = c(1), col = c(7), lwd = 2)
  }
  
}



# example 1

source("./src/fit_stationary_ns_evd_mixture_model.R")
source("./src/initialize_ns_cluster_data.R")
source("./src/estimate_ns_evd_cluster_models.R")

n <- 1000
x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(0.6, 1.3), sd = c(0.1, 0.1))

#x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)

trend <- (-49:50)/n
rnd <- runif(n = n, min = -0.5, max = 0.5)
covariates <- data.frame(trend = trend, random = rnd)

hist(x, nclass = NULL)


nclusters <- 2

initial_cluster_data <- initialize_ns_cluster_data(x = x,
                                                   covariates = covariates,
                                                   nclusters = nclusters)

names(initial_cluster_data)

# initial_cluster_data$cluster_data

# initial_cluster_data$cluster_covariates


cluster_models <- estimate_ns_evd_cluster_models(input_cluster_data = initial_cluster_data,
                                                 location.fun = ~ trend + random,
                                                 scale.fun = ~ 1,
                                                 shape.fun = ~1)

cluster_models


prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))

prior_cluster_weights


evd_mixture_model_object <- fit_stationary_ns_evd_mixture_model(x = x,
                                                                covariates = covariates,
                                                                location.fun = ~ trend + random,
                                                                scale.fun = ~ 1,
                                                                shape.fun = ~1,
                                                                nclusters = 2,
                                                                centers = NULL,
                                                                minimum_cluster_size = 20,
                                                                iteration_max = 500,
                                                                prior_cluster_weights = NULL,
                                                                confidence_level = 0.95)

names(results)


results$iteration

results$cluster_models

results$cluster_attractors_frequencies

results$cluster_attractors_centers

results$cluster_attractors_weights

results$cluster_models_coefficients

results$loglik

results$cluster_information_criteria

results$cluster_models_parameters_variation


plot_fit_non_stationary_evd_mixture_model(evd_mixture_model_object,
                                          data_index = 0,
                                          nclass = NULL,
                                          display_all = FALSE,
                                          xlab = "support",
                                          ylab = "density",
                                          main = "density plot",
                                          legend_position = "topleft")


plot_fit_non_stationary_evd_mixture_model(evd_mixture_model_object,
                                          data_index = 100,
                                          nclass = NULL,
                                          display_all = TRUE,
                                          xlab = "support",
                                          ylab = "density",
                                          main = "density plot",
                                          legend_position = "topleft")

