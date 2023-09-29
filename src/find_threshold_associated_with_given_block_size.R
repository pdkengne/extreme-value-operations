source("./src/extract_block_maxima.R")

find_threshold_associated_with_given_block_size <- function(x, block_size){
  # x: vector of observations
  # block_size: size of blocks to consider
  
  data_lower_bound <- min(x)
  
  block_maxima <- extract_block_maxima(x, block_size)
  
  block_maxima_lower_bound <- min(block_maxima)
  
  if (block_maxima_lower_bound == data_lower_bound){
    threshold <- data_lower_bound
  }
  else{
    threshold <- max(x[x < block_maxima_lower_bound])
  }
  
  threshold
}


# # example 1
# 
# x <- 1:30
# x
# 
# result <- find_threshold_associated_with_given_block_size(x, block_size = 1)
# result
# 
# 
# # example 2
# 
# x <- 1:30
# x
# 
# result <- find_threshold_associated_with_given_block_size(x, block_size = 10)
# result
# 
# 
# # example 3
# 
# x <- rnorm(n = 20)
# x
# 
# result <- find_threshold_associated_with_given_block_size(x, block_size = 10)
# result
