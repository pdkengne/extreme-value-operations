source("./src/find_minimum_block_size.R")
source("./src/find_block_size_associated_with_given_number_of_blocks.R")

get_candidate_block_sizes <- function(x, m = 50){
  # x: vector of observations
  # m: desired maximum number of blocks
  
  # get block size range
  maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m)
  minimum_block_size <- find_minimum_block_size(x)
  
  # deduce candidate block sizes
  candidate_block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
  
  
  candidate_block_sizes
}


# # example 1
# 
# x <- rnorm(n = 1000)
# m <- 40
# 
# result <- get_candidate_block_sizes(x, m)
# 
# result
# 
# 
# # example 2
# 
# x <- rnorm(n = 1000)
# m <- 25
# 
# result <- get_candidate_block_sizes(x, m)
# 
# result
