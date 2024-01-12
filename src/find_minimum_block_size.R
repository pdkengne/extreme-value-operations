#library(zoo)
#library(EnvStats)

source("./src/calculate_mode.R")


find_minimum_block_size <- function(x, threshold = NULL){
  # x: vector of observations
  # threshold: lower bound of block maxima
  
  if (is.null(threshold)){
    # threshold <- max(c(median(x), calculate_mode(x)))
    threshold <- calculate_mode(x)
  }
 
  block_size <- max(diff(which(x > threshold))) + 1
  
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
# 
# result
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
# 
# result
# 
# 
# # example 5
# 
# x <- EnvStats::rzmnorm(n = 1000, mean = 0, sd = 1, p.zero = 0.5)
# 
# threshold <- NULL
# threshold
# 
# result <- find_minimum_block_size(x, threshold = threshold)
# 
# result

