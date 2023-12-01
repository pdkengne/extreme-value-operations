# library(zoo)

calculate_modes <- function(x){
  # x: a vector of numerical values
  
  # create an empty output object
  output <- list()
  
  # get the number of observations
  n <- length(x)
  
  # get the number of equally spaced points at which the density is to be estimated
  p <- floor(log(n)/log(2))
  m <- max(c(512, 2^(p-2)))
  
  # estimate the empirical density
  density_object <- density(x, n = m)
  
  # extract the empirical density
  pdf <- density_object$y
  
  # extract the support of the empirical density
  support <- density_object$x
  
  # find the density maxima and their respective arguments
  max_series <- zoo::rollapply(pdf, width = 2, FUN = max)
  max_series_diff <- diff(max_series)
  max_series_diff_zero_positions <- which(max_series_diff == 0)
  density_max <- pdf[max_series_diff_zero_positions]
  density_argmax <- support[max_series_diff_zero_positions]
  
  # find the density minima and their respective arguments
  min_series <- zoo::rollapply(pdf, width = 2, FUN = min)
  min_series_diff <- diff(min_series)
  min_series_diff_zero_positions <- which(min_series_diff == 0)
  density_min <- pdf[min_series_diff_zero_positions]
  density_argmin <- support[min_series_diff_zero_positions]
  
  # update the output object
  output[["denity_values"]] <- pdf
  output[["density_support"]] <- support
  output[["density_maxima"]] <- density_max
  output[["density_minima"]] <- density_min
  output[["density_maxima_argument"]] <- density_argmax
  output[["density_minima_argument"]] <- density_argmin
  
  output
}


# # example 1
# 
# x <- rnorm(n = 100)
# 
# results <- calculate_modes(x = x)
# 
# results
# 
# 
# # example 2
# 
# x <- bmixture::rmixnorm(n = 10000, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))
# 
# results <- calculate_modes(x = x)
# 
# results


