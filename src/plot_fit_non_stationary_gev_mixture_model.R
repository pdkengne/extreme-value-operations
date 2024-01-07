# library(extRemes)
# library(dplyr)

source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/calculate_modes.R")
source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_cdf.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")
source("./src/get_several_ns_gev_model_normalized_parameters.R")


plot_fit_non_stationary_gev_mixture_model.R <- function(ns_gev_mixture_model_object,
                                                        data_index = 0,
                                                        model_index = 0,
                                                        iterations = 10,
                                                        xlab = "support",
                                                        ylab = "density",
                                                        main = "density plot",
                                                        legend_position = "topright"){
  # ns_gev_mixture_model_object: an object associated with a result of the function "fit_non_stationary_gev_mixture_model()"
  # model_index: indicates the index of the model to display. Possible values are 0, 1, 2, ... 
  # data_index: indicates the index of the data to display. Possible values are 0, 1, 2, ...
  # iterations: number of iterations to perform in the the dichotomy algorithm
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  # legend_position: position of the legend
  
  weights <- ns_gev_mixture_model_object$weights
  
  if (model_index > length(weights) | model_index < 0){
    stop(paste0("Sorry, the value of the model_index argument must be a positive integer smaller than or equal to: ", length(weights)))
  }
  
  model_index <- floor(model_index)
  
  partial_data <- ns_gev_mixture_model_object$partial_data
  
  partial_data_covariates <- ns_gev_mixture_model_object$partial_data_covariates
  
  if (data_index > nrow(partial_data_covariates) | data_index < 0){
    stop(paste0("Sorry, the value of the data_index argument must be a positive integer smaller than or equal to: ", nrow(partial_data_covariates)))
  }
  
  data_index <- floor(data_index)
  
  if (data_index == 0){
    index <- which.max(partial_data)
    data <- dplyr::slice(partial_data_covariates, index)
  }
  else{
    data <- dplyr::slice(partial_data_covariates, data_index)
  }
  
  selected_ns_gev_models <- ns_gev_mixture_model_object$selected_ns_gev_models
  
  selected_full_ns_gev_models <- ns_gev_mixture_model_object$selected_full_ns_gev_models
  
  threshold <- ns_gev_mixture_model_object$threshold
  
  if (ns_gev_mixture_model_object$use_extremal_index){
    normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                data = data,
                                                                                use_extremal_index = TRUE,
                                                                                normalize_parameters = TRUE)
  } 
  else{
    normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                data = data,
                                                                                use_extremal_index = FALSE,
                                                                                normalize_parameters = TRUE)
  }
  
  normalized_gev_parameters <- do.call(rbind, normalized_gev_parameters)
  
  shapes <- normalized_gev_parameters$shape
  scales <- normalized_gev_parameters$scale
  locations <- normalized_gev_parameters$location
  
  several_standard_uniform_residuals <- lapply(selected_ns_gev_models, function(model){
    standard_gumbel_residuals <- extRemes::trans(model)
    standard_uniform_residuals <- calculate_gev_cdf(q = standard_gumbel_residuals,
                                                    loc = 0, 
                                                    scale = 1, 
                                                    shape = 0)
    standard_uniform_residuals
  })
  
  unified_standard_uniform_residuals <- unlist(several_standard_uniform_residuals)
  
  unnormalized_data_1 <- calculate_gev_mixture_model_inverse_cdf(p = unified_standard_uniform_residuals,
                                                                 locations = locations,
                                                                 scales =  scales,
                                                                 shapes = shapes,
                                                                 weights = weights,
                                                                 kind = c("geometric", "arithmetic")[1],
                                                                 iterations = iterations)
  
  unnormalized_data_2 <- calculate_gev_mixture_model_inverse_cdf(p = unified_standard_uniform_residuals,
                                                                 locations = locations,
                                                                 scales =  scales,
                                                                 shapes = shapes,
                                                                 weights = weights,
                                                                 kind = c("geometric", "arithmetic")[2],
                                                                 iterations = iterations)
  
  modes_object_1 <- calculate_modes(x = unnormalized_data_1)
  modes_object_2 <- calculate_modes(x = unnormalized_data_2)
  
  support_1 <- modes_object_1$density_support 
  support_2 <- modes_object_2$density_support
  
  empirical_density_1 <- modes_object_1$density_values
  empirical_density_2 <- modes_object_2$density_values
  
  theoretical_densities_1 <- calculate_gev_mixture_model_pdf(x = support_1,
                                                             locations = locations,
                                                             scales =  scales,
                                                             shapes = shapes,
                                                             weights = weights,
                                                             kind = c("geometric", "arithmetic")[1])
  
  theoretical_densities_2 <- calculate_gev_mixture_model_pdf(x = support_2,
                                                             locations = locations,
                                                             scales =  scales,
                                                             shapes = shapes,
                                                             weights = weights,
                                                             kind = c("geometric", "arithmetic")[2])
  
  modes_object_0 <- calculate_modes(x = partial_data)
  support_0 <- modes_object_0$density_support
  empirical_density_0 <- modes_object_0$density_values
  
  shapes_weighted_mean <- sum(shapes*weights)
  scales_weighted_mean <- sum(scales*weights)
  locations_weighted_mean <- sum(locations*weights)
  
  unnormalized_data_00 <- calculate_gev_inverse_cdf(p = unified_standard_uniform_residuals,
                                                    loc = locations_weighted_mean, 
                                                    scale = scales_weighted_mean, 
                                                    shape = shapes_weighted_mean)
  
  modes_object_00 <- calculate_modes(x = unnormalized_data_00)
  
  support_00 <- modes_object_00$density_support
  
  empirical_density_00 <- modes_object_00$density_values
  
  theoretical_densities_00 <- calculate_gev_pdf(x = support_00, 
                                                loc = locations_weighted_mean, 
                                                scale = scales_weighted_mean, 
                                                shape = shapes_weighted_mean)
  
  
  if (model_index != 0){
    unnormalized_data_3 <- calculate_gev_inverse_cdf(p = unified_standard_uniform_residuals,
                                                     loc = locations[model_index], 
                                                     scale = scales[model_index], 
                                                     shape = shapes[model_index])
    
    modes_object_3 <- calculate_modes(x = unnormalized_data_3)
    
    support_3 <- modes_object_3$density_support
    
    empirical_density_3 <- modes_object_3$density_values
    
    theoretical_densities_3 <- calculate_gev_pdf(x = support_3, 
                                                 loc = locations[model_index], 
                                                 scale = scales[model_index], 
                                                 shape = shapes[model_index])
    
    densities <- c(empirical_density_0, empirical_density_1, empirical_density_2, empirical_density_3, 
                   theoretical_densities_1, theoretical_densities_2, theoretical_densities_3,
                   theoretical_densities_00, empirical_density_00)
    
    support <- c(support_0, support_00, support_1, support_2, support_3)
    
    plot(x = support_1, 
         y = empirical_density_1, 
         type = "l",
         lty = "dotted",
         ylim = range(densities),
         xlim = range(support),
         xlab = xlab, 
         ylab = ylab, 
         main = main, 
         col = 6,
         lwd = 2)  
    
    lines(support_00, empirical_density_00, col = 4, lwd = 2, lty = "dotted")
    lines(support_00, theoretical_densities_00, col = 4, lwd = 2)
    lines(support_0, empirical_density_0, col = 1, lwd = 2)
    lines(support_2, empirical_density_2, col = 7, lwd = 2, lty = "dotted")
    lines(support_1, theoretical_densities_1, col = 6, lwd = 2)
    lines(support_2, theoretical_densities_2, col = 7, lwd = 2)
    lines(support_3, empirical_density_3, col = 3, lwd = 2, lty = "dotted")
    lines(support_3, theoretical_densities_3, col = 3, lwd = 2)
    abline(h = 0, lty = "dotted")
    abline(v = threshold, lty = "dotted")
    
    legend(legend_position, 
           legend = c("empirical_density", "empirical_model", "empirical_harmonic", "empirical_geometric", "empirical_arithmetic", 
                      "theoretical_model", "theoretical_harmonic", "theoretical_geometric", "theoretical_arithmetic"),
           lty = c(1, 2, 2, 2, 2, 1, 1, 1), col = c(1, 3, 4, 6, 7, 3, 4, 6, 7), lwd = 2)
  }
  else{
    densities <- c(empirical_density_0, empirical_density_1, empirical_density_2, 
                   theoretical_densities_1, theoretical_densities_2,
                   theoretical_densities_00, empirical_density_00)
    
    support <- c(support_0, support_00, support_1, support_2)
    
    plot(x = support_1, 
         y = empirical_density_1, 
         type = "l",
         lty = "dotted",
         ylim = range(densities),
         xlim = range(support),
         xlab = xlab, 
         ylab = ylab, 
         main = main, 
         col = 6,
         lwd = 2)  
    
    lines(support_00, empirical_density_00, col = 4, lwd = 2, lty = "dotted")
    lines(support_00, theoretical_densities_00, col = 4, lwd = 2)
    lines(support_0, empirical_density_0, col = 1, lwd = 2)
    lines(support_2, empirical_density_2, col = 7, lwd = 2, lty = "dotted")
    lines(support_1, theoretical_densities_1, col = 6, lwd = 2)
    lines(support_2, theoretical_densities_2, col = 7, lwd = 2)
    abline(h = 0, lty = "dotted")
    abline(v = threshold, lty = "dotted")
    
    legend(legend_position, 
           legend = c("empirical_density", "empirical_harmonic", "empirical_geometric", "empirical_arithmetic", 
                      "theoretical_harmonic", "theoretical_geometric", "theoretical_arithmetic"),
           lty = c(1, 2, 2, 2, 1, 1), col = c(1, 4, 6, 7, 4, 6, 7), lwd = 2)
  }
  
}



# # example 1
# 
# source("./src/fit_non_stationary_gev_mixture_model.R")
# source("./src/generate_gev_sample.R")
# 
# #x <- rnorm(n = 3000)
# 
# #x <- rexp(n = 3000)
# 
# n <- 10000
# 
# loc <- 0
# scale <- 1
# shape <- 0.01
# 
# x <- generate_gev_sample(n = n, loc = loc, scale = scale, shape = shape)
# 
# #x <- rnorm(n)
# 
# ns_gev_mixture_model_object <- fit_non_stationary_gev_mixture_model(x = x,
#                                                                     data = NULL,
#                                                                     location.fun = ~ 1,
#                                                                     scale.fun = ~ 1,
#                                                                     shape.fun = ~ 1,
#                                                                     use.phi = FALSE,
#                                                                     nlargest = 3000,
#                                                                     block_sizes = NULL,
#                                                                     minimum_nblocks = 50,
#                                                                     threshold = NULL,
#                                                                     confidence_level = 0.95,
#                                                                     use_extremal_index = TRUE,
#                                                                     use_uniform_prior = TRUE,
#                                                                     method = c("MLE", "GMLE")[1])
# 
# names(ns_gev_mixture_model_object)
# 
# 
# ns_gev_mixture_model_object$unnormalized_gev_parameters_object
# ns_gev_mixture_model_object$full_normalized_gev_parameters_object
# ns_gev_mixture_model_object$selected_ns_gev_coefficients
# ns_gev_mixture_model_object$weights
# ns_gev_mixture_model_object$threshold
# 
# plot_fit_non_stationary_gev_mixture_model.R(ns_gev_mixture_model_object,
#                                             data_index = 0,
#                                             model_index = 0,
#                                             iterations = 10,
#                                             xlab = "support",
#                                             ylab = "density",
#                                             main = "density plot",
#                                             legend_position = "topright")
# 
# plot_fit_non_stationary_gev_mixture_model.R(ns_gev_mixture_model_object,
#                                             data_index = 0,
#                                             model_index = 5,
#                                             iterations = 10,
#                                             xlab = "support",
#                                             ylab = "density",
#                                             main = "density plot",
#                                             legend_position = "topright")
# 
# 
# # example 2
# 
# source("./src/fit_non_stationary_gev_mixture_model.R")
# source("./src/generate_gev_sample.R")
# 
# 
# n <- 10000
# 
# x <- rnorm(n = n)
# 
# #x <- bmixture::rmixnorm(n = n, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))
# 
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# 
# ns_gev_mixture_model_object <- fit_non_stationary_gev_mixture_model(x = x,
#                                                                     data = data,
#                                                                     location.fun = ~ .,
#                                                                     scale.fun = ~ 1,
#                                                                     shape.fun = ~ 1,
#                                                                     use.phi = FALSE,
#                                                                     nlargest = 3000,
#                                                                     block_sizes = NULL,
#                                                                     minimum_nblocks = 50,
#                                                                     threshold = NULL,
#                                                                     confidence_level = 0.95,
#                                                                     use_extremal_index = TRUE,
#                                                                     use_uniform_prior = FALSE,
#                                                                     method = c("MLE", "GMLE")[1])
# 
# names(ns_gev_mixture_model_object)
# 
# ns_gev_mixture_model_object$unnormalized_gev_parameters_object
# ns_gev_mixture_model_object$full_normalized_gev_parameters_object
# ns_gev_mixture_model_object$selected_ns_gev_coefficients
# ns_gev_mixture_model_object$weights
# ns_gev_mixture_model_object$threshold
# 
# plot_fit_non_stationary_gev_mixture_model.R(ns_gev_mixture_model_object,
#                                             data_index = 0,
#                                             model_index = 0,
#                                             iterations = 10,
#                                             xlab = "support",
#                                             ylab = "density",
#                                             main = "density plot",
#                                             legend_position = "topright")
# 
# plot_fit_non_stationary_gev_mixture_model.R(ns_gev_mixture_model_object,
#                                             data_index = 0,
#                                             model_index = 1,
#                                             iterations = 10,
#                                             xlab = "support",
#                                             ylab = "density",
#                                             main = "density plot",
#                                             legend_position = "topright")

