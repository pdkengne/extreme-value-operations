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

source("./src/find_minimum_block_size.R")
source("./src/find_block_size_associated_with_given_number_of_blocks.R")

x <- rnorm(n = 1000)

minimum_block_size <- find_minimum_block_size(x)
minimum_block_size

maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
maximum_block_size

block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)

results <- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)

head(results)
tail(results)

matplot(rownames(results), results, type = "l")










