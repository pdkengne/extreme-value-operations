# library(zoo)

plot_modes <- function(modes_object,
                       xlab = "support",
                       ylab = "pdf",
                       main = "density plot"){
  # modes_object: an object associated with a result of the function "calculate_modes()"
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  pdf <- modes_object$denity_values
  support <- modes_object$density_support
  density_maxima <- modes_object$density_maxima
  density_minima <- modes_object$density_minima
  density_maxima_argument <- modes_object$density_maxima_argument
  density_minima_argument <- modes_object$density_minima_argument
  
  
  plot(support, pdf, type = "l", xlab = xlab, ylab = ylab, main = main, lwd = 2)
  
  abline(h = density_maxima, lty = "dotted", col = 2, lwd = 2)
  abline(h = density_minima, lty = "dotted", col = 4, lwd = 2)
  
  abline(v = density_maxima_argument, lty = "dotted", col = 2, lwd = 2)
  abline(v = density_minima_argument, lty = "dotted", col = 4, lwd = 2)
  
  abline(h = 0, lty = "dotted", lwd = 2)
  
}


# # example 1
# 
# source("./src/calculate_modes.R")
# 
# x <- rnorm(n = 1000)
# 
# modes_object <- calculate_modes(x = x)
# 
# plot_modes(modes_object)
# 
# 
# 
# # example 2
# 
# x <- bmixture::rmixnorm(n = 10000, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))
# 
# modes_object <- calculate_modes(x = x)
# 
# plot_modes(modes_object)     


