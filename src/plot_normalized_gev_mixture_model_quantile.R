source("./src/calculate_gev_mixture_model_inverse_cdf.R")

plot_normalized_gev_mixture_model_quantile <- function(x, 
                                                       locations, 
                                                       scales, 
                                                       shapes, 
                                                       weights,
                                                       zoom = FALSE,
                                                       threshold = NULL,
                                                       xlab = "Theoretical Quantile", 
                                                       ylab = "Empirical Quantile", 
                                                       main = "Quantile Plot"){
  
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
  empirical_quantiles <- empirical_probability_object$Order.Statistics
  
  # extract empirical cdf
  empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
  
  # calculate theoretical quantiles
  theoretical_quantiles <- calculate_gev_mixture_model_inverse_cdf(p = empirical_cdf, 
                                                                   locations = locations, 
                                                                   scales = scales, 
                                                                   shapes = shapes, 
                                                                   weights = weights,
                                                                   iterations = 25)
  
  # get the quantile range
  quantile_range <- range(c(theoretical_quantiles, empirical_quantiles)) 
  
  # define the non null threshold to use
  if (is.null(threshold)){
    threshold <- min(uvdata)
  }
  
  # get positions of large quantities
  position_large_quantities <- which(empirical_quantiles >= threshold)
  
  # extract large quantities
  large_empirical_quantiles <- empirical_quantiles[position_large_quantities]
  large_empirical_cdf <- empirical_cdf[position_large_quantities]
  large_theoretical_quantiles <- theoretical_quantiles[position_large_quantities]
  large_quantile_range <- range(c(large_theoretical_quantiles, large_empirical_quantiles))
  
  if (zoom){
    # plot quantile
    plot(x = large_theoretical_quantiles, 
         y = large_empirical_quantiles, 
         type = "p", 
         ylim = large_quantile_range,
         xlim = large_quantile_range,
         pch = 20,
         col = 4,
         lwd = 2,
         cex.axis = 1,
         cex.lab = 1,
         cex.main = 1,
         xlab = xlab,
         ylab = ylab,
         main = paste(main,": zoom =", zoom)
    )
  }
  else{
    # plot quantile
    plot(x = theoretical_quantiles, 
         y = empirical_quantiles, 
         type = "p", 
         ylim = quantile_range,
         xlim = quantile_range,
         pch = 20,
         col = 4,
         lwd = 2,
         cex.axis = 1,
         cex.lab = 1,
         cex.main = 1,
         xlab = xlab,
         ylab = ylab,
         main = paste(main,": zoom =", zoom)
    )
  }
  
  abline(a = 0, b = 1, lty = "dotted", lwd = 2)
  lines(large_theoretical_quantiles, large_empirical_quantiles, col = 2, lwd = 2)
  
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
# plot_normalized_gev_mixture_model_quantile(x = z,
#                                            locations = locations,
#                                            scales = scales,
#                                            shapes = shapes,
#                                            weights = weights,
#                                            zoom = FALSE,
#                                            threshold = NULL,
#                                            xlab = "Theoretical Quantile",
#                                            ylab = "Empirical Quantile",
#                                            main = "Quantile Plot")
# 
# threshold <- median(z)
# 
# plot_normalized_gev_mixture_model_quantile(x = z,
#                                            locations = locations,
#                                            scales = scales,
#                                            shapes = shapes,
#                                            weights = weights,
#                                            zoom = FALSE,
#                                            threshold = threshold,
#                                            xlab = "Theoretical Quantile",
#                                            ylab = "Empirical Quantile",
#                                            main = "Quantile Plot")
# 
# plot_normalized_gev_mixture_model_quantile(x = z,
#                                            locations = locations,
#                                            scales = scales,
#                                            shapes = shapes,
#                                            weights = weights,
#                                            zoom = TRUE,
#                                            threshold = threshold,
#                                            xlab = "Theoretical Quantile",
#                                            ylab = "Empirical Quantile",
#                                            main = "Quantile Plot")
