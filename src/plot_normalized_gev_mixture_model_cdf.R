# library(EnvStats)

source("./src/calculate_gev_mixture_model_cdf.R")

plot_normalized_gev_mixture_model_cdf <- function(x, 
                                                  locations, 
                                                  scales, 
                                                  shapes, 
                                                  weights, 
                                                  kind = c("geometric", "arithmetic")[1],
                                                  zoom = FALSE,
                                                  threshold = NULL,
                                                  xlab = "Quantile", 
                                                  ylab = "Cumulative Probability",
                                                  main = "Cumulative Distribution Function (CDF) Plot"){
  # x: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  # zoom: a boolean which indicates whether to focus on large values or not
  # threshold: smallest value above which to perform comparison. If not provided, comparison is performed on all data
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract train data
  uvdata <- x
  
  # calculate empirical cdf
  empirical_probability_object <- EnvStats::ecdfPlot(x = uvdata, prob.method ="plot.pos", plot.pos.con = 0.375, plot.it = FALSE)
  
  # extract ordered quantiles
  ordered_quantiles <- empirical_probability_object$Order.Statistics
  
  # extract empirical cdf
  empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
  
  # calculate theoretical cdf
  theoretical_cdf <- calculate_gev_mixture_model_cdf(q = ordered_quantiles, 
                                                     locations = locations, 
                                                     scales = scales, 
                                                     shapes = shapes, 
                                                     weights = weights,
                                                     kind = kind)
  
  # get the cdf range
  cdf_range <- range(c(theoretical_cdf, empirical_cdf))
  
  # define the non null threshold to use
  if (is.null(threshold)){
    threshold <- min(uvdata)
  }
  
  # get positions of large quantities
  position_large_quantities <- which(ordered_quantiles >= threshold)
  
  # extract large quantities
  large_ordered_quantiles <- ordered_quantiles[position_large_quantities]
  large_empirical_cdf <- empirical_cdf[position_large_quantities]
  large_theoretical_cdf <- theoretical_cdf[position_large_quantities]
  large_cdf_range <- range(c(large_theoretical_cdf, large_empirical_cdf))
  
  # plot densities
  if (zoom){
    plot(x = large_ordered_quantiles, 
         y = large_empirical_cdf, 
         type = "l", 
         ylim = large_cdf_range,
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
         y = empirical_cdf, 
         type = "l", 
         ylim = cdf_range,
         col = 4,
         lwd = 2,
         cex.axis = 1,
         cex.lab = 1,
         cex.main = 1,
         xlab = xlab,
         ylab = ylab,
         main = paste(main,": zoom =", zoom))
  }
  
  lines(large_ordered_quantiles, large_theoretical_cdf, col = 2, lwd = 2)
  
  abline(h = 0, lty = "dotted", lwd = 1)
  abline(v = threshold, lty = "dotted", lwd = 1)
  
  legend(x = "bottomright", 
         legend = c("Empirical cdf", "Theoretical cdf"),
         lty = c(1, 1),
         col = c(4, 2),
         lwd = 2,
         title = "Legend"
  )
  
}



# example 1

source("./src/generate_gev_sample.R")
source("./src/estimate_gev_mixture_model_parameters.R")
source("./src/find_threshold_associated_with_given_block_size.R")

n <- 10000
nlargest <- 1000

# x <- rnorm(n = n)
x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.1)

gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
                                                           block_sizes = NULL,
                                                           minimum_nblocks = 50,
                                                           nlargest = nlargest,
                                                           confidence_level = 0.95,
                                                           log_mv = FALSE,
                                                           log_pw = FALSE,
                                                           trace = TRUE)

# extract the model parameters (mw)
gev_mixture_model_parameters <- gev_mixture_model$normalized_gev_parameters_object
gev_mixture_model_parameters_shape <- gev_mixture_model_parameters$shape_star
gev_mixture_model_parameters_scale <- gev_mixture_model_parameters$scale_star
gev_mixture_model_parameters_loc <- gev_mixture_model_parameters$loc_star

# extract the weight parameters (mw)
gev_mixture_model_identic_weights <- gev_mixture_model$identic_weights_mw
gev_mixture_model_pessimistic_weights <- gev_mixture_model$pessimistic_weights_mw
gev_mixture_model_automatic_weights <- gev_mixture_model$automatic_weights_mw

y <- gev_mixture_model$data_largest

plot_normalized_gev_mixture_model_cdf(x = y,
                                      locations = gev_mixture_model_parameters_loc,
                                      scales = gev_mixture_model_parameters_scale,
                                      shapes = gev_mixture_model_parameters_shape,
                                      weights = gev_mixture_model_identic_weights,
                                      kind = c("geometric", "arithmetic")[1],
                                      zoom = FALSE,
                                      threshold = NULL,
                                      xlab = "Quantile",
                                      ylab = "Cumulative Probability",
                                      main = "Cumulative Distribution Function (CDF) Plot")

plot_normalized_gev_mixture_model_cdf(x = y,
                                      locations = gev_mixture_model_parameters_loc,
                                      scales = gev_mixture_model_parameters_scale,
                                      shapes = gev_mixture_model_parameters_shape,
                                      weights = gev_mixture_model_pessimistic_weights,
                                      kind = c("geometric", "arithmetic")[1],
                                      zoom = FALSE,
                                      threshold = NULL,
                                      xlab = "Quantile",
                                      ylab = "Cumulative Probability",
                                      main = "Cumulative Distribution Function (CDF) Plot")

block_size <- max(gev_mixture_model$block_sizes)

threshold <- find_threshold_associated_with_given_block_size(x = y, block_size = block_size)

plot_normalized_gev_mixture_model_cdf(x = y,
                                      locations = gev_mixture_model_parameters_loc,
                                      scales = gev_mixture_model_parameters_scale,
                                      shapes = gev_mixture_model_parameters_shape,
                                      weights = gev_mixture_model_automatic_weights,
                                      kind = c("geometric", "arithmetic")[1],
                                      zoom = FALSE,
                                      threshold = threshold,
                                      xlab = "Quantile",
                                      ylab = "Cumulative Probability",
                                      main = "Cumulative Distribution Function (CDF) Plot")

plot_normalized_gev_mixture_model_cdf(x = y,
                                      locations = gev_mixture_model_parameters_loc,
                                      scales = gev_mixture_model_parameters_scale,
                                      shapes = gev_mixture_model_parameters_shape,
                                      weights = gev_mixture_model_automatic_weights,
                                      kind = c("geometric", "arithmetic")[1]
                                      zoom = TRUE,
                                      threshold = threshold,
                                      xlab = "Quantile",
                                      ylab = "Cumulative Probability",
                                      main = "Cumulative Distribution Function (CDF) Plot")
