generate_equivalent_thresholds <- function(x, threshold, n = 1){
  # x: vector of observations
  # threshold: a threshold value
  # n: number of equivalent thresholds to generate
  #    Here, two thresholds are said to be equivalent if they yield the same set of peaks over threshold.
  
  lower_value <- max(x[x <= threshold])
  upper_value <- min(x[x > threshold])
  
  equivalent_threshold <- runif(n, min = lower_value, max = upper_value)
  
  equivalent_threshold
}


# # example 1
# 
# x <- 1:20
# x
# 
# results <- generate_equivalent_thresholds(x, threshold = 10, n = 1)
# 
# results
# 
# 
# # example 2
# 
# x <- 1:30
# x
# 
# results <- generate_equivalent_thresholds(x, threshold = 19, n = 5)
# 
# results
# 
# 
# # example 3
# 
# x <- rnorm(n = 1000)
# 
# results <- generate_equivalent_thresholds(x, threshold = 0, n = 1)
# 
# results
