# library(extRemes)

source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/calculate_modes.R")
source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_cdf.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")


plot_fit_stationary_gev_mixture_model <- function(gev_mixture_model_object,
                                                  model_index = 0,
                                                  zoom_thresholds = c(-Inf, +Inf),
                                                  xlab = "support",
                                                  ylab = "density",
                                                  main = "density plot",
                                                  legend_position = "topright"){
  # gev_mixture_model_object: an object associated with a result of the function "fit_stationary_gev_mixture_model()"
  # model_index: indicates the index of the model to display. Possible values are 0, 1, 2, ... 
  # zoom_thresholds: indicates the lower and/or upper bound of the support to display
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  # legend_position: position of the legend
  
  weights <- gev_mixture_model_object$weights
  
  if (model_index > length(weights) | model_index < 0){
    stop(paste0("Sorry, the value of the model_index argument must be a positive integer smaller than or equal to: ", length(weights)))
  }
  
  model_index <- floor(model_index)
  
  selected_gev_models <- gev_mixture_model_object$selected_gev_models
  
  threshold <- gev_mixture_model_object$threshold
  
  partial_data <- gev_mixture_model_object$partial_data
  
  if (gev_mixture_model_object$use_extremal_index){
    normalized_gev_parameters <- gev_mixture_model_object$full_normalized_gev_parameters_object
  } 
  else{
    normalized_gev_parameters <- gev_mixture_model_object$normalized_gev_parameters_object
  }
  
  shapes <- normalized_gev_parameters$shape_star
  scales <- normalized_gev_parameters$scale_star
  locations <- normalized_gev_parameters$loc_star
  
  size <- 1000
  
  if (!is.infinite(zoom_thresholds[1]) & is.infinite(zoom_thresholds[2])){
    support_1 <- seq(from = zoom_thresholds[1], to = max(partial_data), length.out = size) 
  }
  else if (is.infinite(zoom_thresholds[1]) & !is.infinite(zoom_thresholds[2])){
    support_1 <- seq(from = min(partial_data), to = zoom_thresholds[2], length.out = size) 
  }
  else if (!is.infinite(zoom_thresholds[1]) & !is.infinite(zoom_thresholds[2])){
    support_1 <- seq(from = zoom_thresholds[1], to = zoom_thresholds[2], length.out = size) 
  }
  else{
    support_1 <- seq(from = min(partial_data), to = max(partial_data), length.out = size) 
  }
  
  theoretical_densities_1 <- calculate_gev_mixture_model_pdf(x = support_1,
                                                             locations = locations,
                                                             scales =  scales,
                                                             shapes = shapes,
                                                             weights = weights,
                                                             kind = c("geometric", "arithmetic", "harmonic")[1])
  
  theoretical_densities_2 <- calculate_gev_mixture_model_pdf(x = support_1,
                                                             locations = locations,
                                                             scales =  scales,
                                                             shapes = shapes,
                                                             weights = weights,
                                                             kind = c("geometric", "arithmetic", "harmonic")[2])
  
  theoretical_densities_3 <- calculate_gev_mixture_model_pdf(x = support_1,
                                                             locations = locations,
                                                             scales =  scales,
                                                             shapes = shapes,
                                                             weights = weights,
                                                             kind = c("geometric", "arithmetic", "harmonic")[3])
  
  modes_object_0 <- calculate_modes(x = partial_data)
  support_0 <- modes_object_0$density_support
  empirical_density_0 <- modes_object_0$density_values
  
  if (!is.infinite(zoom_thresholds[1]) & is.infinite(zoom_thresholds[2])){
    positions <- support_0 > zoom_thresholds[1]
    support_0 <- support_0[positions]
    empirical_density_0 <- empirical_density_0[positions]
  }
  else if (is.infinite(zoom_thresholds[1]) & !is.infinite(zoom_thresholds[2])){
    positions <- support_0 < zoom_thresholds[2]
    support_0 <- support_0[positions]
    empirical_density_0 <- empirical_density_0[positions]
  }
  else if (!is.infinite(zoom_thresholds[1]) & !is.infinite(zoom_thresholds[2])){
    positions <- (support_0 > zoom_thresholds[1]) & (support_0 < zoom_thresholds[2])
    support_0 <- support_0[positions]
    empirical_density_0 <- empirical_density_0[positions]
  }
  
  
  if (model_index != 0){
    
    if (!is.infinite(zoom_thresholds[1]) & is.infinite(zoom_thresholds[2])){
      support_00 <- seq(from = zoom_thresholds[1], to = max(partial_data), length.out = size)
    }
    else if (is.infinite(zoom_thresholds[1]) & !is.infinite(zoom_thresholds[2])){
      support_00 <- seq(from = min(partial_data), to = zoom_thresholds[2], length.out = size)
    }
    else if (!is.infinite(zoom_thresholds[1]) & !is.infinite(zoom_thresholds[2])){
      support_00 <- seq(from = zoom_thresholds[1], to = zoom_thresholds[2], length.out = size)
    }
    else{
      support_00 <- seq(from = min(partial_data), to = max(partial_data), length.out = size)
    }
    
    theoretical_densities_00 <- calculate_gev_pdf(x = support_00, 
                                                  loc = locations[model_index], 
                                                  scale = scales[model_index], 
                                                  shape = shapes[model_index])
    
    densities <- c(empirical_density_0, theoretical_densities_00, theoretical_densities_1, theoretical_densities_2, theoretical_densities_3)
    
    support <- c(support_0, support_00, support_1)
    
    
    plot(x = support_0, 
         y = empirical_density_0, 
         type = "l",
         ylim = range(densities),
         xlim = range(support),
         xlab = xlab, 
         ylab = ylab, 
         main = main, 
         col = 1,
         lwd = 2)  
    
    lines(support_00, theoretical_densities_00, col = 3, lwd = 2)
    lines(support_1, theoretical_densities_1, col = 6, lwd = 2)
    lines(support_1, theoretical_densities_2, col = 7, lwd = 2)
    lines(support_1, theoretical_densities_3, col = 4, lwd = 2)
    abline(h = 0, lty = "dotted")
    abline(v = threshold, lty = "dotted")
    abline(v = zoom_thresholds, lty = "dotted", col = )
    
    legend(legend_position, 
           legend = c("empirical_density", "theoretical_model", "theoretical_harmonic", "theoretical_geometric", "theoretical_arithmetic"),
           lty = c(1, 1, 1, 1, 1), col = c(1, 3, 4, 6, 7), lwd = 2)
  }
  else{
    densities <- c(empirical_density_0, theoretical_densities_1, theoretical_densities_2, theoretical_densities_3)
    
    support <- c(support_0, support_1, support_1, support_1)
    
    plot(x = support_0, 
         y = empirical_density_0, 
         type = "l",
         ylim = range(densities),
         xlim = range(support),
         xlab = xlab, 
         ylab = ylab, 
         main = main, 
         col = 1,
         lwd = 2)  
    
    lines(support_1, theoretical_densities_1, col = 6, lwd = 2)
    lines(support_1, theoretical_densities_2, col = 7, lwd = 2)
    lines(support_1, theoretical_densities_3, col = 4, lwd = 2)
    
    abline(h = 0, lty = "dotted")
    abline(v = threshold, lty = "dotted")
    abline(v = zoom_thresholds, lty = "dotted")
    
    legend(legend_position, 
           legend = c("empirical_density", "theoretical_harmonic", "theoretical_geometric", "theoretical_arithmetic"),
           lty = c(1, 1, 1, 1), col = c(1, 4, 6, 7), lwd = 2)
  }
  
}



# # example 1
# 
# source("./src/fit_stationary_gev_mixture_model.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 2000
# 
# #x <- bmixture::rmixnorm(n = n, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))
# 
# x <- rnorm(n = n)
# 
# x <- rexp(n = n, rate = 1)
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.5)
# 
# 
# hist(x, nclass = 30, probability = TRUE)
# 
# 
# y <- x[x > median(x)]
# 
# gev_mixture_model_object <- fit_stationary_gev_mixture_model(x = y,
#                                                              nlargest = Inf,
#                                                              block_sizes = NULL,
#                                                              minimum_nblocks = 50,
#                                                              threshold = NULL,
#                                                              confidence_level = 0.95,
#                                                              use_extremal_index = TRUE,
#                                                              use_uniform_prior = TRUE,
#                                                              method = c("MLE", "GMLE", "Lmoments")[1])
# 
# gev_mixture_model_object$unnormalized_gev_parameters_object
# gev_mixture_model_object$full_normalized_gev_parameters_object
# gev_mixture_model_object$weights
# gev_mixture_model_object$threshold
# gev_mixture_model_object$information_criteria
# 
# plot_fit_stationary_gev_mixture_model(gev_mixture_model_object,
#                                       model_index = 0,
#                                       zoom_thresholds = c(-Inf, +Inf),
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
# 
# 
# plot_fit_stationary_gev_mixture_model(gev_mixture_model_object,
#                                       model_index = 11,
#                                       zoom_thresholds = c(-Inf, +Inf),
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
# 
# 
# plot_fit_stationary_gev_mixture_model(gev_mixture_model_object,
#                                       model_index = 6,
#                                       zoom_thresholds = c(-Inf, +Inf),
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
# 
# 
# plot_fit_stationary_gev_mixture_model(gev_mixture_model_object,
#                                       model_index = 6,
#                                       zoom_thresholds = c(1, +Inf),
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
# 
# 
# # example 2
# 
# source("./src/fit_stationary_gev_mixture_model.R")
# source("./src/generate_gev_mixture_model_sample.R")
# 
# weights <- c(1/3, 1/3, 1/3)
# 
# shapes <- c(0.01, 0.01, 0.01)
# scales <- c(1, 1, 1)
# locations <- c(-2, +2, +6)
# 
# n <- 3000
# 
# x <- generate_gev_mixture_model_sample(n = n,
#                                        locations,
#                                        scales,
#                                        shapes,
#                                        weights,
#                                        kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# gev_mixture_model_object <- fit_stationary_gev_mixture_model(x = x,
#                                                              nlargest = 3000,
#                                                              block_sizes = NULL,
#                                                              minimum_nblocks = 50,
#                                                              threshold = NULL,
#                                                              confidence_level = 0.95,
#                                                              use_extremal_index = TRUE,
#                                                              use_uniform_prior = TRUE,
#                                                              method = c("MLE", "GMLE", "Lmoments")[1])
# 
# gev_mixture_model_object$unnormalized_gev_parameters_object
# gev_mixture_model_object$full_normalized_gev_parameters_object
# gev_mixture_model_object$weights
# gev_mixture_model_object$threshold
# 
# plot_fit_stationary_gev_mixture_model(gev_mixture_model_object,
#                                       model_index = 0,
#                                       zoom_thresholds = c(-Inf, +Inf),
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
# 
# 
# plot_fit_stationary_gev_mixture_model(gev_mixture_model_object,
#                                       model_index = 0,
#                                       zoom_thresholds = c(8, +Inf),
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
