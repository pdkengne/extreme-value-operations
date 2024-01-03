# library(extRemes)

source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/calculate_modes.R")
source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")


plot_fit_stationary_gev_mixture_model <- function(gev_mixture_model_object,
                                                  model_index = 1,
                                                  xlab = "support",
                                                  ylab = "density",
                                                  main = "density plot",
                                                  legend_position = "topright"){
  # gev_mixture_model_object: an object associated with a result of the function "fit_stationary_gev_mixture_model()"
  # model_index: indicates the index of the model to display. Possible values are 1, 2, ... 
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  # legend_position: position of the legend
  
  weights <- gev_mixture_model_object$weights
  
  if (model_index > length(weights)){
    stop(paste0("Sorry, the value of the index argument must be smaller than or equal to: ", length(weights)))
  }
  
  threshold <- gev_mixture_model_object$threshold
  
  if (gev_mixture_model_object$use_extremal_index){
    normalized_gev_parameters <- gev_mixture_model_object$full_normalized_gev_parameters_object
  } 
  else{
    normalized_gev_parameters <- gev_mixture_model_object$normalized_gev_parameters_object
  }
  
  shapes <- normalized_gev_parameters$shape_star
  scales <- normalized_gev_parameters$scale_star
  locations <- normalized_gev_parameters$loc_star
  
  selected_model <- gev_mixture_model_object$selected_gev_models[[model_index]]
  
  normalized_data <- extRemes::trans(selected_model)
  
  unnormalized_data <- extRemes::revtrans.evd(z = normalized_data,
                                              location = locations[model_index], 
                                              scale = scales[model_index], 
                                              shape = shapes[model_index],
                                              type = "GEV")
  
  modes_object <- calculate_modes(x = unnormalized_data)
  
  support <- modes_object$density_support
  
  empirical_density <- modes_object$denity_values
  
  theoretical_densities <- calculate_gev_pdf(x = support, 
                                             loc = locations[model_index], 
                                             scale = scales[model_index], 
                                             shape = shapes[model_index])
  
  theoretical_densities_1 <- calculate_gev_mixture_model_pdf(x = support,
                                                             locations = locations,
                                                             scales =  scales,
                                                             shapes = shapes,
                                                             weights = weights,
                                                             kind = c("geometric", "arithmetic")[1])
  
  theoretical_densities_2 <- calculate_gev_mixture_model_pdf(x = support,
                                                             locations = locations,
                                                             scales =  scales,
                                                             shapes = shapes,
                                                             weights = weights,
                                                             kind = c("geometric", "arithmetic")[2])
  
  densities <- c(empirical_density, theoretical_densities, theoretical_densities_1, theoretical_densities_2)
  
  plot(x = support, 
       y = empirical_density, 
       type = "l",
       ylim = range(densities),
       xlab = xlab, 
       ylab = ylab, 
       main = main, 
       lwd = 2)  
  
  lines(support, theoretical_densities, col = 3, lwd = 2)
  lines(support, theoretical_densities_1, col = 6, lwd = 2)
  lines(support, theoretical_densities_2, col = 7, lwd = 2)
  abline(h = 0, lty = "dotted")
  abline(v = threshold, lty = "dotted")
  
  legend(legend_position, 
         legend = c("empirical", "theoretical", "geometric", "arithmetic"),
         lty = c(1, 1, 1, 1), col = c(1, 3, 6, 7))

}



# # example 1
# 
# source("./src/fit_stationary_gev_mixture_model.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 10000
# 
# #x <- bmixture::rmixnorm(n = n, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))
# 
# x <- rnorm(n = n)
# 
# #x <- rexp(n = n, rate = 1)
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.01)
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
#                                       model_index = 1,
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
# n <- 10000
# 
# x <- generate_gev_mixture_model_sample(n = n,
#                                        locations,
#                                        scales,
#                                        shapes,
#                                        weights,
#                                        kind = c("geometric", "arithmetic")[2])
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
#                                       model_index = 2,
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
