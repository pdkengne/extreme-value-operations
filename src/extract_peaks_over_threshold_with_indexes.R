extract_peaks_over_threshold_with_indexes <- function(x, threshold){
  # x: vector of observations
  # threshold: threshold to consider
  
  # create an empty output object
  output <- list()
  
  # get the total number of observations
  n <- length(x)
  
  # add index to the vector of observations
  names(x) <- 1:n
  
  # get the peaks over threshold
  peaks_over_threshold <- x[x > threshold]
  
  # get the peaks over threshold indexes
  peaks_over_threshold_indexes <- names(peaks_over_threshold)
  
  # update the output object
  output[["peaks_over_threshold"]] <- peaks_over_threshold
  output[["peaks_over_threshold_indexes"]] <- peaks_over_threshold_indexes
  
  output
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
