source("./src/find_minimum_block_size.R")
source("./src/extract_block_maxima.R")

find_minimum_threshold <- function(x) {
  # x: vector of observations
  
  block_size <- find_minimum_block_size(x)
  block_maxima <- extract_block_maxima(x, block_size)
  threshold <- max(median(x), max(x[x < min(block_maxima)]))
  
  threshold
}


# # example 1
# 
# x <- rexp(n = 1000)
# median(x)
# 
# result <- find_minimum_threshold(x)
# 
# result
# 
# 
# # example 2
# 
# x <- rnorm(n = 1000)
# median(x)
# 
# result <- find_minimum_threshold(x)
# 
# result
# 
# 
# # example 3
# 
# x <- c(rnorm(n = 1000), rep(0, 100))
# median(x)
# 
# result <- find_minimum_threshold(x)
# 
# result
# 
# 
# # example 4
# 
# x <- EnvStats::rzmnorm(n = 1000, mean = 0, sd = 1, p.zero = 0.5)
# median(x)
# 
# result <- find_minimum_threshold(x)
# 
# result

