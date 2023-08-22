extract_block_maxima <- function(x, block_size = 1){
  # x: vector of observations
  # block_size: size of blocks to consider
  
  # get the total number of observations
  n <- length(x)
  
  # get the number of blocks
  m <- ceiling(n/block_size)
  
  # initialize the vector of block maxima
  block_maxima <- rep(NA, m)
  
  # update the first block maxima value
  block_maxima[1] <- max(x[1:block_size])
  
  # update the last block maxima value
  block_maxima[m] <- max(x[((m-1)*block_size + 1):n])
  
  # update the intermediate block maxima values
  block_maxima[2:(m-1)]<- sapply(1:(m-2), function(k) max(x[(k*block_size + 1):((k + 1)*block_size)]))
  

  block_maxima
}


# # example 1
# 
# x <- rnorm(n = 1000)
# 
# results <- extract_block_maxima(x, block_size = 50)
# 
# results
# 
# 
# # example 2
# 
# x <- 1:1000
# 
# results <- extract_block_maxima(x, block_size = 50)
# 
# results


