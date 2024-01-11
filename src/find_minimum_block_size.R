#library(zoo)
#library(EnvStats)

source("./src/calculate_mode.R")


find_minimum_block_size <- function(x, threshold = NULL){
  # x: vector of observations
  # threshold: lower bound of block maxima
  
  if (is.null(threshold)){
    # med <- median(x)
    # mod <- calculate_mode(x, data_type = c("continuous", "discrete")[1])
    # threshold = max(med, mod)
    threshold = min(x)
  }
  
  block_size <- 1
  y <- zoo::rollmax(x, k = block_size)
  
  while (min(y, na.rm = TRUE) < threshold){
    block_size <- block_size + 1
    y <- zoo::rollmax(x, k = block_size)
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
# result
