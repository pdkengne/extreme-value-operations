#library(zoo)

find_block_size_associated_with_given_threshold <- function(x, threshold){
  # x: vector of observations
  # threshold: threshold to consider
  
  block_size <- 2
  y <- zoo::rollmax(x, k = block_size)
  
  while (min(y) <= threshold){
    block_size <- block_size + 1
    y <- zoo::rollmax(x, k = block_size)
  }
  
  block_size
}


# # example 1
# 
# x <- 1:30
# x
# 
# result <- find_block_size_associated_with_given_threshold(x, threshold = 9)
# result
# 
# 
# # example 2
# 
# x <- rnorm(n = 20)
# x
# 
# result <- find_block_size_associated_with_given_threshold(x, threshold = 0)
# result
