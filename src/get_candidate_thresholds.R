source("./src/find_minimum_threshold.R")
source("./src/find_threshold_associated_with_given_number_of_largest_values.R")

get_candidate_thresholds <- function(x, k = 50){
  # x: vector of observations
  # k: desired minimum number of largest values
  
  # get thresholds range
  minimum_threshold <- find_minimum_threshold(x)
  maximum_threshold <- find_threshold_associated_with_given_number_of_largest_values(x, k)
  
  # deduce candidate thresholds
  candidate_thresholds <- sort(x[(x >= minimum_threshold) & (x <= maximum_threshold)])
  
  candidate_thresholds
}


# # example 1
# 
# x <- 1:100
# x
# 
# result <- get_candidate_thresholds(x, k = 20)
# result
# 
# 
# # example 2
# 
# x <- rnorm(n = 1000)
# 
# result <- get_candidate_thresholds(x, k = 50)
# result
