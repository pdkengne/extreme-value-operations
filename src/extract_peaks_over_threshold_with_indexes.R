extract_peaks_over_threshold_with_indexes <- function(x, threshold){
  # x: vector of observations
  # threshold: threshold to consider
  
  # get the total number of observations
  n <- length(x)
  
  # add index to the vector of observations
  names(x) <- 1:n
  
  # get the peaks over threshold
  peaks_over_threshold <- x[x > threshold]
  
  peaks_over_threshold
}


# # example 1
# 
# x <- rnorm(n = 100)
# 
# results <- extract_peaks_over_threshold_with_indexes(x, threshold = 0)
# 
# results
# names(results)
# 
# 
# # example 2
# 
# x <- 1:100
# 
# results <- extract_peaks_over_threshold_with_indexes(x, threshold = 80)
# 
# results
# names(results)
