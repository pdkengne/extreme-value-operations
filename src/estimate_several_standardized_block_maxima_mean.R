source("./src/estimate_single_standardized_block_maxima_mean.R")
source("./src/extract_largest_subset_of_overlapping_intervals.R")

estimate_several_standardized_block_maxima_mean <- function(x, block_sizes, confidence_level = 0.95){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # confidence_interval_level: desired confidence level
  
  # create an empty output object
  output <- list()
  
  # estimate the confidence intervals
  estimated_mean_confidence_intervals <- sapply(block_sizes, 
                                                function(block_size) 
                                                  estimate_single_standardized_block_maxima_mean(x, block_size, confidence_level))
  
  estimated_mean_confidence_intervals <- data.frame(t(estimated_mean_confidence_intervals))
  rownames(estimated_mean_confidence_intervals) <- block_sizes
  
  # find the selector of the largest subset of intervals which overlap
  selector_object <- extract_largest_subset_of_overlapping_intervals(estimated_mean_confidence_intervals[c(1, 3)])
  selector <- selector_object$interval_selector
  
  # extract equivalent estimates
  selected <- estimated_mean_confidence_intervals[selector, ]
  
  # extract unselected estimates
  rejected <- estimated_mean_confidence_intervals[!selector, ]

  # update the output object
  output[["estimates"]] <- estimated_mean_confidence_intervals
  output[["selected"]] <- selected
  output[["rejected"]] <- rejected
  
  output 
}


# # example 1
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# 
# x <- rnorm(n = 10000)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# results <- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# 
# names(results)
# 
# # all estimates
# results$estimates
# 
# # equivalent estimates
# results$selected
# 
# # unselected estimates
# results$rejected
# 
# 
# 
# # example 2
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = 0)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# results <- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# 
# names(results)
# 
# # all estimates
# results$estimates
# 
# # equivalent estimates
# results$selected
# 
# # unselected estimates
# results$rejected
# 
# 
# # example 3
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = -0.2)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# results <- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# 
# names(results)
# 
# # all estimates
# results$estimates
# 
# # equivalent estimates
# results$selected
# 
# # unselected estimates
# results$rejected
