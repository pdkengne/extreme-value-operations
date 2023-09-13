# library(EnvStats)

source("./src/calculate_gev_cdf.R")

plot_normalized_gev_cdf <- function(x, 
                                    loc = 0, 
                                    scale = 1, 
                                    shape = 0,
                                    zoom = FALSE,
                                    threshold = NULL,
                                    xlab = "Quantile", 
                                    ylab = "Cumulative Probability",
                                    main = "Cumulative Distribution Function (CDF) Plot"){
  # x: vector of observations
  # loc, scale, shape: normalized location, scale and shape parameters of the considered gev distribution
  # threshold: smallest value above which to perform comparison. If not provided, comparison is performed on all data
  # zoom: a boolean which indicates whether to focus on large values or not
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
  theoretical_cdf <- calculate_gev_cdf(q = ordered_quantiles, 
                                       loc = loc, 
                                       scale = scale, 
                                       shape = shape)
  
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
         legend = c("Empirical CDF", "Theoretical CDF"),
         lty = c(1, 1),
         col = c(4, 2),
         lwd = 2,
         title = "Legend"
  )
  
}


# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_single_gev_model.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)
# 
# block_size <- 1
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# # plot_gev_cdf(gev_model)
# 
# gev_model_parameters <- gev_model$normalized_gev_parameters
# 
# plot_normalized_gev_cdf(x,
#                         loc = gev_model_parameters["loc_star"],
#                         scale = gev_model_parameters["scale_star"],
#                         shape = gev_model_parameters["shape_star"],
#                         zoom = FALSE,
#                         threshold = NULL,
#                         xlab = "Quantile",
#                         ylab = "Cumulative Probability",
#                         main = "Cumulative Distribution Function (CDF) Plot")
# 
# plot_normalized_gev_cdf(x,
#                         loc = gev_model_parameters["loc_star"],
#                         scale = gev_model_parameters["scale_star"],
#                         shape = gev_model_parameters["shape_star"],
#                         zoom = FALSE,
#                         threshold = 2,
#                         xlab = "Quantile",
#                         ylab = "Cumulative Probability",
#                         main = "Cumulative Distribution Function (CDF) Plot")
# 
# plot_normalized_gev_cdf(x,
#                         loc = gev_model_parameters["loc_star"],
#                         scale = gev_model_parameters["scale_star"],
#                         shape = gev_model_parameters["shape_star"],
#                         zoom = TRUE,
#                         threshold = 2,
#                         xlab = "Quantile",
#                         ylab = "Cumulative Probability",
#                         main = "Cumulative Distribution Function (CDF) Plot")
