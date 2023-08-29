source("./src/extract_block_maxima.R")

find_threshold_associated_with_given_block_size <- function(x, block_size = 1){
  # x: vector of observations
  # block_size: size of blocks to consider
  
  block_maxima <- extract_block_maxima(x, block_size)
  
  threshold <- max(x[x < min(block_maxima)])
  
  threshold
}


# example 1

x <- 1:30
x

result <- find_threshold_associated_with_given_block_size(x, block_size = 10)
result


# example 2

x <- rnorm(n = 20)
x

result <- find_threshold_associated_with_given_block_size(x, block_size = 10)
result


