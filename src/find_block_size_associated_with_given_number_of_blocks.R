find_block_size_associated_with_given_number_of_blocks <- function(x, m = 50){
  # x: vector of observations
  # m: desired minimum number of blocks
  
  size <- length(x)
  block_size <- floor(size/m)
  
  block_size
}


# # example 1
# 
# x <- rnorm(n = 1000)
# m <- 40
# 
# result <- find_block_size_associated_with_given_number_of_blocks(x, m)
# 
# result
# 
# 
# # example 2
# 
# x <- rnorm(n = 1000)
# m <- 25
# 
# result <- find_block_size_associated_with_given_number_of_blocks(x, m)
# 
# result
