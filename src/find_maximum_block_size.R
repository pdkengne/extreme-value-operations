find_maximum_block_size <- function(x, m = 50){
  # x: vector of observations
  # m: minimum number of blocks
  
  size <- length(x)
  block_size <- floor(size/m)
  
  block_size
}

# examples

# x <- rnorm(n = 1000)
# m <- 40
# 
# result <- find_maximum_block_size(x, m)
# 
# result
