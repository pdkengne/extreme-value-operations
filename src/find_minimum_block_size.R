#library(zoo)
#library(EnvStats)

source("./src/calculate_mode.R")
source("./src/extract_block_maxima.R")


find_minimum_block_size <- function(x, threshold = NULL){
  # x: vector of observations
  # threshold: lower bound of block maxima
  
  if (is.null(threshold)){
    block_size <- 1
  }
  else {
    block_size <- max(diff(which(x > threshold))) + 1
    maxima <- extract_block_maxima(x, block_size = block_size)
    
    while(min(maxima) <= threshold){
      block_size <- block_size + 1
      maxima <- extract_block_maxima(x, block_size = block_size)
    }
    
  }
 
  block_size
}


# # example 1
# 
# x <- rexp(n = 1000)
# 
# threshold <- median(x)
# threshold
# 
# result <- find_minimum_block_size(x, threshold = threshold)
# result
# 
# maxima <- extract_block_maxima(x, block_size = result)
# 
# min(maxima) > threshold
# 
# 
# # example 2
# 
# x <- rnorm(n = 1000)
# 
# threshold <- median(x)
# threshold
# 
# result <- find_minimum_block_size(x, threshold = threshold)
# 
# result
# 
# maxima <- extract_block_maxima(x, block_size = result)
# 
# min(maxima) > threshold
# 
# 
# # example 3
# 
# x <- c(rnorm(n = 1000), rep(0, 100))
# 
# threshold <- median(x)
# threshold
# 
# result <- find_minimum_block_size(x, threshold = threshold)
# 
# result
# 
# maxima <- extract_block_maxima(x, block_size = result)
# 
# min(maxima) > threshold
# 
# 
# # example 4
# 
# x <- EnvStats::rzmnorm(n = 1000, mean = 0, sd = 1, p.zero = 0.5)
# 
# threshold <- median(x)
# threshold
# 
# result <- find_minimum_block_size(x, threshold = threshold)
# 
# result
# 
# maxima <- extract_block_maxima(x, block_size = result)
# 
# min(maxima) > threshold
# 
# 
# # example 5
# 
# x <- EnvStats::rzmnorm(n = 1000, mean = 0, sd = 1, p.zero = 0.5)
# 
# threshold <- 0
# threshold
# 
# result <- find_minimum_block_size(x, threshold = threshold)
# 
# result
# 
# maxima <- extract_block_maxima(x, block_size = result)
# 
# min(maxima) > threshold
