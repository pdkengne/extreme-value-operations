# library(EnvStats)

source("./src/calculate_gev_mixture_model_pdf.R")

plot_normalized_gev_mixture_model_pdf <- function(x, 
                                                  locations, 
                                                  scales, 
                                                  shapes, 
                                                  weights, 
                                                  zoom = FALSE,
                                                  threshold = NULL,
                                                  xlab = "Quantile", 
                                                  ylab = "Density", 
                                                  main = "Probability Density Function (PDF) Plot"){
  # x: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # zoom: a boolean which indicates whether to focus on large values or not
  # threshold: smallest value above which to perform comparison. If not provided, comparison is performed on all data
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract train data
  uvdata <- x
  
  # calculate empirical pdf
  empirical_density_object <- EnvStats::epdfPlot(x = uvdata, plot.it = FALSE)
  
  # extract ordered quantiles
  ordered_quantiles <- empirical_density_object$x
  
  # extract empirical pdf
  empirical_pdf <- empirical_density_object$f.x
  
  # calculate theoretical pdf
  theoretical_pdf <- calculate_gev_mixture_model_pdf(x = ordered_quantiles, 
                                                     locations = locations, 
                                                     scales = scales, 
                                                     shapes = shapes, 
                                                     weights = weights)
  
  # get the pdf range
  pdf_range <- range(c(theoretical_pdf, empirical_pdf))
  
  # define the non null threshold to use
  if (is.null(threshold)){
    threshold <- min(uvdata)
  }
  
  # get positions of large quantities
  position_large_quantities <- which(ordered_quantiles >= threshold)
  
  # extract large quantities
  large_ordered_quantiles <- ordered_quantiles[position_large_quantities]
  large_empirical_pdf <- empirical_pdf[position_large_quantities]
  large_theoretical_pdf <- theoretical_pdf[position_large_quantities]
  large_pdf_range <- range(c(large_theoretical_pdf, large_empirical_pdf))
  
  # plot densities
  if (zoom){
    plot(x = large_ordered_quantiles, 
         y = large_empirical_pdf, 
         type = "l", 
         ylim = large_pdf_range,
         col = 4,
         lwd = 2,
         cex.axis = 1,
         cex.lab = 1,
         cex.main = 1,
         xlab = xlab,
         ylab = ylab,
         main = paste(main,": zoom =", zoom))
  }
  else{
    plot(x = ordered_quantiles, 
         y = empirical_pdf, 
         type = "l", 
         ylim = pdf_range,
         col = 4,
         lwd = 2,
         cex.axis = 1,
         cex.lab = 1,
         cex.main = 1,
         xlab = xlab,
         ylab = ylab,
         main = paste(main,": zoom =", zoom))
  }
  
  lines(large_ordered_quantiles, large_theoretical_pdf, col = 2, lwd = 2)
  
  abline(h = 0, lty = "dotted", lwd = 1)
  abline(v = threshold, lty = "dotted", lwd = 1)
  
  legend(x = "topright", 
         legend = c("Empirical PDF", "Theoretical PDF"),
         lty = c(1, 1),
         col = c(4, 2),
         lwd = 2,
         title = "Legend"
  )
  
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_gev_mixture_model_parameters.R")
# source("./src/find_threshold_associated_with_given_block_size.R")
# 
# n <- 10000
# nlargest <- 1000
# 
# # x <- rnorm(n = n)
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.1)
# 
# gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
#                                                            nsloc = NULL,
#                                                            std.err = FALSE,
#                                                            block_sizes = NULL,
#                                                            minimum_nblocks = 50,
#                                                            nlargest = nlargest,
#                                                            confidence_level = 0.95,
#                                                            log_mv = FALSE,
#                                                            log_pw = FALSE,
#                                                            trace = TRUE)
# 
# # extract the model parameters (mw)
# gev_mixture_model_parameters <- gev_mixture_model$normalized_gev_parameters_object
# gev_mixture_model_parameters_shape <- gev_mixture_model_parameters$shape_star
# gev_mixture_model_parameters_scale <- gev_mixture_model_parameters$scale_star
# gev_mixture_model_parameters_loc <- gev_mixture_model_parameters$loc_star
# 
# # extract the weight parameters (mw)
# gev_mixture_model_identic_weights <- gev_mixture_model$identic_weights_mw
# gev_mixture_model_pessimistic_weights <- gev_mixture_model$pessimistic_weights_mw
# gev_mixture_model_automatic_weights <- gev_mixture_model$automatic_weights_mw
# 
# y <- gev_mixture_model$data_largest
# 
# plot_normalized_gev_mixture_model_pdf(x = y,
#                                       locations = gev_mixture_model_parameters_loc,
#                                       scales = gev_mixture_model_parameters_scale,
#                                       shapes = gev_mixture_model_parameters_shape,
#                                       weights = gev_mixture_model_identic_weights,
#                                       zoom = FALSE,
#                                       threshold = NULL,
#                                       xlab = "Quantile",
#                                       ylab = "Density",
#                                       main = "Probability Density Function (PDF) Plot")
# 
# plot_normalized_gev_mixture_model_pdf(x = y,
#                                       locations = gev_mixture_model_parameters_loc,
#                                       scales = gev_mixture_model_parameters_scale,
#                                       shapes = gev_mixture_model_parameters_shape,
#                                       weights = gev_mixture_model_pessimistic_weights,
#                                       zoom = FALSE,
#                                       threshold = NULL,
#                                       xlab = "Quantile",
#                                       ylab = "Density",
#                                       main = "Probability Density Function (PDF) Plot")
# 
# block_size <- max(gev_mixture_model$block_sizes)
# 
# threshold <- find_threshold_associated_with_given_block_size(x = y, block_size = block_size)
# 
# plot_normalized_gev_mixture_model_pdf(x = y,
#                                       locations = gev_mixture_model_parameters_loc,
#                                       scales = gev_mixture_model_parameters_scale,
#                                       shapes = gev_mixture_model_parameters_shape,
#                                       weights = gev_mixture_model_automatic_weights,
#                                       zoom = FALSE,
#                                       threshold = threshold,
#                                       xlab = "Quantile",
#                                       ylab = "Density",
#                                       main = "Probability Density Function (PDF) Plot")
# 
# plot_normalized_gev_mixture_model_pdf(x = y,
#                                       locations = gev_mixture_model_parameters_loc,
#                                       scales = gev_mixture_model_parameters_scale,
#                                       shapes = gev_mixture_model_parameters_shape,
#                                       weights = gev_mixture_model_automatic_weights,
#                                       zoom = TRUE,
#                                       threshold = threshold,
#                                       xlab = "Quantile",
#                                       ylab = "Density",
#                                       main = "Probability Density Function (PDF) Plot")
# 
