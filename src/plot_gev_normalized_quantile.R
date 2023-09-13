# library(EnvStats)

source("./src/calculate_gev_inverse_cdf.R")

plot_gev_normalized_quantile <- function(x, 
                                         loc = 0, 
                                         scale = 1, 
                                         shape = 0,  
                                         xlab = "Theoretical Quantile", 
                                         ylab = "Empirical Quantile", 
                                         main = "Quantile Plot"){
  # x: vector of observations
  # loc, scale, shape: normalized location, scale and shape parameters of the considered gev distribution
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract train data
  uvdata <- x
  
  # calculate empirical cdf
  empirical_probability_object <- EnvStats::ecdfPlot(x = uvdata, prob.method ="plot.pos", plot.pos.con = 0.375, plot.it = FALSE)
  
  # extract empirical quantiles
  empirical_quantiles <- empirical_probability_object$Order.Statistics
  
  # extract empirical cdf
  empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
  
  # calculate theoretical quantiles
  theoretical_quantiles <- calculate_gev_inverse_cdf(p = empirical_cdf, 
                                                     loc = loc, 
                                                     scale = scale, 
                                                     shape = shape)
  
  # get the quantile range
  quantile_range <- range(c(theoretical_quantiles, empirical_quantiles)) 
  
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
# loc <- 1
# 
# scale <- 0.5
# 
# shape <- 0.1
# 
# x <- generate_gev_sample(n = 1000, loc = loc, scale = scale, shape = shape)
# 
# plot_gev_normalized_quantile(x, 
#                              loc = loc, 
#                              scale = scale, 
#                              shape = shape,  
#                              xlab = "Theoretical Quantile", 
#                              ylab = "Empirical Quantile", 
#                              main = "Quantile Plot")
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# loc <- 1
# 
# scale <- 0.5
# 
# shape <- -0.2
# 
# x <- generate_gev_sample(n = 1000, loc = loc, scale = scale, shape = shape)
# 
# plot_gev_normalized_quantile(x, 
#                              loc = loc, 
#                              scale = scale, 
#                              shape = shape,  
#                              xlab = "Theoretical Quantile", 
#                              ylab = "Empirical Quantile", 
#                              main = "Quantile Plot")
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# 
# loc <- 1
# 
# scale <- 0.5
# 
# shape <- 0
# 
# x <- generate_gev_sample(n = 1000, loc = loc, scale = scale, shape = shape)
# 
# plot_gev_normalized_quantile(x, 
#                              loc = loc, 
#                              scale = scale, 
#                              shape = shape,  
#                              xlab = "Theoretical Quantile", 
#                              ylab = "Empirical Quantile", 
#                              main = "Quantile Plot")
