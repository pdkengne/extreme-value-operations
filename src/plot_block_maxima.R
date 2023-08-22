source("extract_block_maxima_with_indexes.R")

plot_block_maxima <- function(x, block_size = 1){
  # x: vector of observations
  # block_size: size of blocks to consider
  
  # get the vector of block maxima
  block_maxima <- extract_block_maxima_with_indexes(x, block_size)
  
  
  
}



# example 1

x <- rnorm(n = 1000)
block_size <- 50
block_maxima <- extract_block_maxima_with_indexes(x, block_size)

block_maxima_indexes <- as.numeric(attributes(block_maxima)$names)

m <- length(block_maxima)

block_intervals <- c(1, 
                     sapply(1:(m-1), function(k) k*block_size),
                     m*block_size)

xlab <- "Index"
ylab <- "Values"
main <- "Block maxima"

plot(x, xaxt = "n", type = "h", col = 4, xlab = xlab, ylab = ylab, main = main, cex.lab = 1, cex.main = 1, cex.axis = 1)
abline(v = block_intervals, lty = "dotted", lwd = 2)
points(block_maxima_indexes, block_maxima, pch = 10, col = 2, lwd = 2)

axis(side = 1, at = block_intervals, labels = block_intervals, las = 2, cex.axis = 1)


