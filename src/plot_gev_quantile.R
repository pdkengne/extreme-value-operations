# library(EnvStats)

source("./src/estimate_single_gev_model.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/find_threshold_associated_with_given_block_size.R")

plot_gev_quantile <- function(gev_model, 
                              xlab = "Theoretical Quantile", 
                              ylab = "Empirical Quantile", 
                              main = "Quantile Plot"){
  # gev_model: an object associated with a result of the function "estimate_single_gev_model()"
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the model parameters
  gev_model_parameters <- gev_model$normalized_gev_parameters
  
  # extract train data
  uvdata <- gev_model$data
  
  # extract block size
  block_size <- gev_model$block_size
  
  # find the threshold associated with the block_size
  threshold <- find_threshold_associated_with_given_block_size(x = uvdata, block_size = block_size)
  
  # calculate empirical cdf
  empirical_probability_object <- EnvStats::ecdfPlot(x = uvdata, prob.method ="plot.pos", plot.pos.con = 0.375, plot.it = FALSE)
  
  # extract empirical quantiles
  empirical_quantiles <- empirical_probability_object$Order.Statistics
  
  # extract empirical cdf
  empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
  
  # calculate theoretical quantiles
  theoretical_quantiles <- calculate_gev_inverse_cdf(p = empirical_cdf, 
                                                     loc = gev_model_parameters["loc_star"], 
                                                     scale = gev_model_parameters["scale_star"], 
                                                     shape = gev_model_parameters["shape_star"])
  
  # get the quantile range
  quantile_range <- range(c(theoretical_quantiles, empirical_quantiles)) 
  
  # get positions of large quantities
  position_large_quantities <- which(empirical_quantiles >= threshold)
  
  # extract large quantities
  large_empirical_quantiles <- empirical_quantiles[position_large_quantities]
  large_theoretical_quantiles <- theoretical_quantiles[position_large_quantities]
  large_quantile_range <- range(c(large_theoretical_quantiles, large_empirical_quantiles))
  
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
       main = main
  )
  
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
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = +0.2)
# 
# block_size <- 100
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_quantile(gev_model, 
#                   xlab = "Theoretical Quantile", 
#                   ylab = "Empirical Quantile", 
#                   main = "Quantile Plot")
# 
# gev_model$normalized_gev_parameters
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = -0.2)
# 
# block_size <- 100
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_quantile(gev_model, 
#                   xlab = "Theoretical Quantile", 
#                   ylab = "Empirical Quantile", 
#                   main = "Quantile Plot")
# 
# gev_model$normalized_gev_parameters
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = 0)
# 
# block_size <- 100
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_quantile(gev_model, 
#                   xlab = "Theoretical Quantile", 
#                   ylab = "Empirical Quantile", 
#                   main = "Quantile Plot")
# 
# gev_model$normalized_gev_parameters
