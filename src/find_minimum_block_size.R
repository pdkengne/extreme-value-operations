#library(zoo)
#library(EnvStats)


find_minimum_block_size <- function(x){
  # x: vector of observations
  
  block_size <- 2
  y <- zoo::rollmax(x, k = block_size)
  
  while (min(y) <= median(x, na.rm = TRUE)){
    block_size <- block_size + 1
    y <- zoo::rollmax(x, k = block_size)
  }
  
  block_size
}


# # example 1
# 
# x <- c(rexp(n = 1000))
# 
# result <- find_minimum_block_size(x)
# 
# result
# 
# 
# # example 2
# 
# x <- c(rnorm(n = 1000))
# 
# result <- find_minimum_block_size(x)
# 
# result
# 
# 
# # example 3
# 
# x <- c(rnorm(n = 1000), rep(0, 100))
# 
# result <- find_minimum_block_size(x)
# 
# result
# 
# 
# # example 4
# 
# x <- EnvStats::rzmnorm(n = 1000, mean = 0, sd = 1, p.zero = 0.5)
# 
# result <- find_minimum_block_size(x)
# 
# result




