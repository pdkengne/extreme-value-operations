source("./src/extract_block_maxima_with_indexes.R")

plot_block_maxima <- function(x, block_size = 1, xlab = "Index", ylab = "Values", main = "Block maxima"){
  # x: vector of observations
  # block_size: size of blocks to consider
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the vector of block maxima
  block_maxima <- extract_block_maxima_with_indexes(x, block_size)
  
  # get the index of block maxima
  block_maxima_indexes <- as.numeric(attributes(block_maxima)$names)
  
  # get the total number of blocks
  m <- length(block_maxima)
  
  # find the index of block intervals 
  block_intervals <- c(1, sapply(1:(m-1), function(k) k*block_size), m*block_size)
  
  # plot the observed values
  plot(x, xaxt = "n", type = "h", col = 4, 
       xlab = xlab, ylab = ylab, main = main, lwd = 1,
       cex.lab = 1, cex.main = 1, cex.axis = 1)
  axis(side = 1, at = block_intervals, labels = block_intervals, las = 2, cex.axis = 1)
  
  # add the block intervals
  abline(v = block_intervals, lty = "dotted", lwd = 2)
  
  # mark the block maxima
  points(block_maxima_indexes, block_maxima, pch = 10, col = 2, lwd = 2)
  
}



# # example 1
# 
# x <- rnorm(n = 1000)
# 
# plot_block_maxima(x, block_size = 40, xlab = "Index", ylab = "Values", main = "Block maxima")
# 
# # example 2
# 
# x <- rexp(n = 1000)
# 
# plot_block_maxima(x, block_size = 40, xlab = "Index", ylab = "Values", main = "Block maxima")
# 
# 
# # example 3
# 
# source("./src/find_minimum_block_size.R")
# 
# x <- EnvStats::rzmnorm(n = 1000, mean = 0, sd = 1, p.zero = 0.5)
# 
# block_size <- find_minimum_block_size(x)
# block_size
# 
# plot_block_maxima(x, block_size = block_size, xlab = "Index", ylab = "Values", main = "Block maxima")
# 
# abline(h = median(x), col = 7, lwd = 1)
# 
# 
# # example 4
# 
# source("./src/find_minimum_block_size.R")
# 
# x <- rgamma(n = 1000, shape = 5, rate = 1)
# 
# block_size <- find_minimum_block_size(x)
# block_size
# 
# plot_block_maxima(x, block_size = block_size, xlab = "Index", ylab = "Values", main = "Block maxima")
# 
# abline(h = median(x), col = 7, lwd = 1)

