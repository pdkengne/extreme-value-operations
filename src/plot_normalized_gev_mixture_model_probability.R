source("./src/calculate_gev_mixture_model_cdf.R")

plot_normalized_gev_mixture_model_probability <- function(x, 
                                                          locations, 
                                                          scales, 
                                                          shapes, 
                                                          weights,
                                                          zoom = FALSE,
                                                          threshold = NULL,
                                                          xlab = "Theoretical Probability", 
                                                          ylab = "Empirical Probability", 
                                                          main = "Probability Plot"){
  
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
                                                     weights = weights)
  
  # get the probability range
  probability_range <- range(c(theoretical_cdf, empirical_cdf))
  
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
  large_probability_range <- range(c(large_theoretical_cdf, large_empirical_cdf))
  
  if (zoom){
    # plot probability
    plot(x = large_theoretical_cdf, 
         y = large_empirical_cdf, 
         type = "p", 
         ylim = large_probability_range,
         xlim = large_probability_range,
         pch = 20,
         col = 4,
         lwd = 2,
         cex.axis = 1,
         cex.lab = 1,
         cex.main = 1,
         xlab = xlab,
         ylab = ylab,
         main = main
    )
  }
  else{
    # plot probability
    plot(x = theoretical_cdf, 
         y = empirical_cdf, 
         type = "p", 
         ylim = probability_range,
         xlim = probability_range,
         pch = 20,
         col = 4,
         lwd = 2,
         cex.axis = 1,
         cex.lab = 1,
         cex.main = 1,
         xlab = xlab,
         ylab = ylab,
         main = main
    )
  }
  
  abline(a = 0, b = 1, lty = "dotted", lwd = 2)
  
  legend(x = "topleft", 
         legend = c("Line y = x"),
         lty = 3,
         col = 1,
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
# block_size <- max(gev_mixture_model$block_sizes)
# 
# threshold <- find_threshold_associated_with_given_block_size(x = y, block_size = block_size)
# 
# y <- gev_mixture_model$data_largest
# 
# plot_normalized_gev_mixture_model_probability(x = y, 
#                                               locations = gev_mixture_model_parameters_loc, 
#                                               scales = gev_mixture_model_parameters_scale, 
#                                               shapes = gev_mixture_model_parameters_shape, 
#                                               weights = gev_mixture_model_automatic_weights,
#                                               zoom = FALSE,
#                                               threshold = NULL,
#                                               xlab = "Theoretical Probability", 
#                                               ylab = "Empirical Probability", 
#                                               main = "Probability Plot")
# 
# plot_normalized_gev_mixture_model_probability(x = y, 
#                                               locations = gev_mixture_model_parameters_loc, 
#                                               scales = gev_mixture_model_parameters_scale, 
#                                               shapes = gev_mixture_model_parameters_shape, 
#                                               weights = gev_mixture_model_automatic_weights,
#                                               zoom = FALSE,
#                                               threshold = threshold,
#                                               xlab = "Theoretical Probability", 
#                                               ylab = "Empirical Probability", 
#                                               main = "Probability Plot")
# 
# plot_normalized_gev_mixture_model_probability(x = y, 
#                                               locations = gev_mixture_model_parameters_loc, 
#                                               scales = gev_mixture_model_parameters_scale, 
#                                               shapes = gev_mixture_model_parameters_shape, 
#                                               weights = gev_mixture_model_automatic_weights,
#                                               zoom = TRUE,
#                                               threshold = threshold,
#                                               xlab = "Theoretical Probability", 
#                                               ylab = "Empirical Probability", 
#                                               main = "Probability Plot")
# 
# 
# # example 2
# 
# source("./src/generate_gev_mixture_model_sample.R")
# 
# m <- 10
# 
# y <- runif(m)
# weights <- y/sum(y)
# 
# shapes <- runif(n = m, min = -0.2, max = -0.1)
# scales <- runif(n = m)
# locations <- rnorm(n = m)
# 
# n <- 1000
# 
# z <- generate_gev_mixture_model_sample(n = n, locations, scales, shapes, weights, iterations = 50)
# 
# 
# plot_normalized_gev_mixture_model_probability(x = z, 
#                                               locations = locations, 
#                                               scales = scales, 
#                                               shapes = shapes, 
#                                               weights = weights,
#                                               zoom = FALSE,
#                                               threshold = threshold,
#                                               xlab = "Theoretical Probability", 
#                                               ylab = "Empirical Probability", 
#                                               main = "Probability Plot")
