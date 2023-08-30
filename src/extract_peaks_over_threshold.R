extract_peaks_over_threshold <- function(x, threshold){
  # x: vector of observations
  # threshold: threshold to consider
  
  # get the peaks over threshold
  peaks_over_threshold <- x[x > threshold]
  
  peaks_over_threshold
}


# # example 1
# 
# x <- rnorm(n = 100)
# 
# results <- extract_peaks_over_threshold(x, threshold = 0)
# 
# results
# 
# 
# # example 2
# 
# x <- 1:100
# 
# results <- extract_peaks_over_threshold(x, threshold = 80)
# 
# results
