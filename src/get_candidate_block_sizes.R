source("./src/find_minimum_block_size.R")
source("./src/find_block_size_associated_with_given_number_of_blocks.R")

get_candidate_block_sizes <- function(x, threshold = NULL, m = 50){
  # x: vector of observations
  # threshold: lower bound of block maxima
  # m: desired minimum number of blocks
  
  # get block size range
  minimum_block_size <- find_minimum_block_size(x, threshold = threshold)
  maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m)
  
  while(minimum_block_size > maximum_block_size & m > 10){
    m <- m - 1
    maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m)
  }
  
  if (maximum_block_size < minimum_block_size){
    # stop(paste("The provided minimum number of blocks,", m, "is too large! Please, consider using a smaller number."))
    stop(paste("The provided threshold,", threshold, "is too large! Please, consider using a smaller value."))
  }
  
  # deduce candidate block sizes
  candidate_block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
  
  candidate_block_sizes
}


# # example 1
# 
# x <- rnorm(n = 1000)
# m <- 40
# threshold <- median(x)
# threshold
# 
# result <- get_candidate_block_sizes(x, threshold = threshold, m = m)
# 
# result
# 
# 
# # example 2
# 
# x <- rnorm(n = 1000)
# m <- 25
# threshold <- median(x)
# threshold
# 
# result <- get_candidate_block_sizes(x, threshold = threshold, m = m)
# 
# result
# 
# 
# # example 3
# 
# x <- rexp(n = 1000)
# m <- 50
# threshold <- 1
# threshold
# 
# result <- get_candidate_block_sizes(x, threshold = threshold, m = m)
# 
# result
