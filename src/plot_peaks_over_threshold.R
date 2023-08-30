source("./src/extract_peaks_over_threshold_with_indexes.R")

plot_peaks_over_threshold <- function(x, threshold, xlab = "Index", ylab = "Values", main = "Peaks over threshold"){
  # x: vector of observations
  # threshold: threshold to consider
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the vector of peaks over threshold with indexes
  peaks_over_threshold_with_indexes <- extract_peaks_over_threshold_with_indexes(x, threshold)
  
  # extract the vector of peaks over threshold
  peaks_over_threshold <- peaks_over_threshold_with_indexes$peaks_over_threshold
  
  # get the index of peaks over threshold
  peaks_over_threshold_indexes <- peaks_over_threshold_with_indexes$peaks_over_threshold_indexes
  
  # plot the observed values
  plot(x, xaxt = "n", type = "h", col = 4, 
       xlab = xlab, ylab = ylab, main = main, lwd = 1,
       cex.lab = 1, cex.main = 1, cex.axis = 1)
  axis(side = 1, at = peaks_over_threshold_indexes, labels = peaks_over_threshold_indexes, las = 2, cex.axis = 1)
  
  # add the threshold line
  abline(h = threshold,  lwd = 1, col = 7)
  
  # mark the peaks over threshold
  points(peaks_over_threshold_indexes, peaks_over_threshold, pch = 16, col = 2, lwd = 2)
  
}



# # example 1
# 
# source("./src/find_threshold_associated_with_given_number_of_largest_values.R")
# 
# x <- rnorm(n = 1000)
# 
# threshold <- find_threshold_associated_with_given_number_of_largest_values(x, k = 50)
# threshold
# 
# plot_peaks_over_threshold(x, threshold = threshold, xlab = "Index", ylab = "Values", main = "Peaks over threshold")
# 
# 
# # example 2
# 
# x <- rexp(n = 1000)
# 
# threshold <- find_threshold_associated_with_given_number_of_largest_values(x, k = 100)
# threshold
# 
# plot_peaks_over_threshold(x, threshold = threshold, xlab = "Index", ylab = "Values", main = "Peaks over threshold")
# 
# 
# # example 3
# 
# source("./src/find_minimum_threshold.R")
# 
# x <- EnvStats::rzmnorm(n = 1000, mean = 0, sd = 1, p.zero = 0.5)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# plot_peaks_over_threshold(x, threshold = threshold, xlab = "Index", ylab = "Values", main = "Peaks over threshold")
# 
# 
# # example 4
# 
# source("./src/find_minimum_block_size.R")
# 
# x <- rgamma(n = 1000, shape = 5, rate = 1)
# 
# threshold <- find_minimum_threshold(x)
# threshold
# 
# plot_peaks_over_threshold(x, threshold = threshold, xlab = "Index", ylab = "Values", main = "Peaks over threshold")
