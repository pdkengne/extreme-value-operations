source("./src/extract_nlargest_sample.R")

find_threshold_associated_with_given_number_of_largest_values <- function(x, k = 50){
  # x: vector of observations
  # k: desired number of largest values
  
  nlargest_sample <- extract_nlargest_sample(x, n = k)
  
  threshold <- max(x[x < min(nlargest_sample)])
  
  threshold
}


# # example 1
# 
# x <- 1:30
# x
# 
# result <- find_threshold_associated_with_given_number_of_largest_values(x, k = 10)
# result
# 
# 
# # example 2
# 
# x <- rnorm(n = 20)
# x
# 
# result <- find_threshold_associated_with_given_number_of_largest_values(x, k = 10)
# result
