extract_block_maxima_with_indexes <- function(x, block_size = 1){
  # x: vector of observations
  # block_size: size of blocks to consider
  
  # get the total number of observations
  n <- length(x)
  
  # get the number of blocks
  m <- ceiling(n/block_size)
  
  # initialize the vector of block maxima
  block_maxima <- rep(NA, m)
  block_maxima_indexes <- rep(NA, m)
  
  # update the first block maxima value
  block_maxima[1] <- max(x[1:block_size])
  block_maxima_indexes[1] <- which.max(x[1:block_size])[1]
  
  # update the last block maxima value
  block_maxima[m] <- max(x[((m-1)*block_size + 1):n])
  block_maxima_indexes[m] <- (m-1)*block_size + which.max(x[((m-1)*block_size + 1):n])[1]
  
  # update the intermediate block maxima values
  block_maxima[2:(m-1)]<- sapply(1:(m-2), function(k) max(x[(k*block_size + 1):((k + 1)*block_size)]))
  block_maxima_indexes[2:(m-1)] <- sapply(1:(m-2), function(k) 
    k*block_size + which.max(x[(k*block_size + 1):((k + 1)*block_size)]))
  
  names(block_maxima) <- block_maxima_indexes
  
  block_maxima
}


# example 1

x <- rnorm(n = 1000)

results <- extract_block_maxima_with_indexes(x, block_size = 50)

results

attributes(results)$names

# example 2

x <- 1:1000

results <- extract_block_maxima_with_indexes(x, block_size = 50)

results

attributes(results)$names
