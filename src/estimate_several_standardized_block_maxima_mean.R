source("./src/estimate_single_standardized_block_maxima_mean.R")

estimate_several_standardized_block_maxima_mean <- function(x, block_sizes, confidence_level = 0.95){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # confidence_interval_level: desired confidence level
  
  estimated_mean_confidence_intervals <- sapply(block_sizes, 
                                                function(block_size) 
                                                  estimate_single_standardized_block_maxima_mean(x, block_size, confidence_level))
  
  estimated_mean_confidence_intervals <- data.frame(t(estimated_mean_confidence_intervals))
  rownames(estimated_mean_confidence_intervals) <- block_sizes
  
  estimated_mean_confidence_intervals
}


# example 1

x <- rnorm(n = 10000)

results <- estimate_several_standardized_block_maxima_mean(x, block_sizes = 1:150, confidence_level = 0.95)

head(results)
tail(results)

matplot(rownames(results), results, type = "l")










