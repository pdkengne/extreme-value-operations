source("./src/estimate_single_ns_standardized_block_maxima_mean.R")
source("./src/extract_largest_subset_of_overlapping_intervals.R")

estimate_several_ns_standardized_block_maxima_mean <- function(x, 
                                                               block_sizes, 
                                                               confidence_level = 0.95, 
                                                               data = NULL, 
                                                               location.fun = ~1,
                                                               scale.fun = ~1, 
                                                               shape.fun = ~1, 
                                                               use.phi = TRUE,
                                                               type = c("GEV", "Gumbel")[1],
                                                               method = c("MLE", "GMLE")[1]){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # confidence_level: desired confidence level
  # data: dataframe of covariates for linear modeling of the location parameter
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # type: type of model to use
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  # estimate the confidence intervals
  estimated_mean_confidence_intervals <- sapply(block_sizes, 
                                                function(block_size) 
                                                  estimate_single_ns_standardized_block_maxima_mean(x = x, 
                                                                                                    block_size = block_size, 
                                                                                                    confidence_level = confidence_level,
                                                                                                    data = data, 
                                                                                                    location.fun = location.fun,
                                                                                                    scale.fun = scale.fun, 
                                                                                                    shape.fun = shape.fun, 
                                                                                                    use.phi = use.phi,
                                                                                                    type = type,
                                                                                                    method = method))
  
  estimated_mean_confidence_intervals <- data.frame(t(estimated_mean_confidence_intervals))
  rownames(estimated_mean_confidence_intervals) <- block_sizes
  
  # find the selector of the largest subset of intervals which overlap
  selector_object <- extract_largest_subset_of_overlapping_intervals(estimated_mean_confidence_intervals[, c(1, 3)])
  selector <- selector_object$interval_selector
  
  # extract a common interval to all intervals from the largest subset of overlapping intervals
  common_interval <- selector_object$common_interval
  
  # extract equivalent estimates
  selected <- estimated_mean_confidence_intervals[selector, ]
  
  # extract unselected estimates
  rejected <- estimated_mean_confidence_intervals[!selector, ]

  # update the output object
  output[["estimates"]] <- estimated_mean_confidence_intervals
  output[["selected"]] <- selected
  output[["rejected"]] <- rejected
  output[["common_interval"]] <- common_interval
  
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
# results <- estimate_several_ns_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, method = c("MLE", "GMLE")[1])
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
# # common interval
# results$common_interval
# 
# 
# # example 2
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 10000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# results <- estimate_several_ns_standardized_block_maxima_mean(x, 
#                                                               block_sizes, 
#                                                               confidence_level = 0.95, 
#                                                               data = data, 
#                                                               location.fun = ~ .,
#                                                               scale.fun = ~1, 
#                                                               shape.fun = ~1, 
#                                                               use.phi = TRUE,
#                                                               type = c("GEV", "Gumbel")[1],
#                                                               method = c("MLE", "GMLE")[2])
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
# # common interval
# results$common_interval
# 
# 
# # example 3
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 10000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# results <- estimate_several_ns_standardized_block_maxima_mean(x, 
#                                                               block_sizes, 
#                                                               confidence_level = 0.95, 
#                                                               data = data, 
#                                                               location.fun = ~ .,
#                                                               scale.fun = ~ ., 
#                                                               shape.fun = ~1, 
#                                                               use.phi = TRUE,
#                                                               type = c("GEV", "Gumbel")[1],
#                                                               method = c("MLE", "GMLE")[1])
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
# # common interval
# results$common_interval

