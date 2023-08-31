source("./src/check_whether_an_element_belongs_to_several_intervals.R")

extract_largest_subset_of_overlapping_intervals<- function(table_of_intervals){
  # function to identify the largest subset of intervals which overlap.
  # Intervals are specified as a table (table_of_intervals) having two columns.
  # In this table, the first and the second components of each row are respectively the lower and upper bounds of an interval.
  
  # create an empty output object
  output <- list()
  
  # number of elements to check whether they belong to the provided intervals
  m <- 200
  
  # vector of elements to check whether they belong to the provided intervals
  elements <- seq(from = min(table_of_intervals[, 1]), to = max(table_of_intervals[, 2]), length.out = m)
  
  # check whether each considered value belongs to the provided intervals
  overlaps <- sapply(elements,
                     function(value)
                       check_whether_an_element_belongs_to_several_intervals(table_of_intervals = table_of_intervals, element = value))
  
  # extract the selector of interval belonging to the largest subset of overlapping intervals
  size_of_overlapping_intervals <- apply(overlaps, 2 , sum, na.rm = TRUE)
  positions_of_values_in_largest_common_interval <-  which(size_of_overlapping_intervals == max(size_of_overlapping_intervals)) 
  
  position_of_smallest_most_common_value <- min(positions_of_values_in_largest_common_interval)
  position_of_greatest_most_common_value <- max(positions_of_values_in_largest_common_interval)
  
  interval_selector <- overlaps[, position_of_smallest_most_common_value]
  
  # extract the largest subset of overlapping intervals
  largest_subset_of_overlapping_intervals <- table_of_intervals[interval_selector, ]
  
  # extract a common value to all intervals from the largest subset of overlapping intervals
  common_interval <- c(elements[position_of_smallest_most_common_value], elements[position_of_greatest_most_common_value])
  names(common_interval) <- c("lower_bound", "upper_bound")
  
  # update the output object
  output[["largest_subset_of_overlapping_intervals"]] <- largest_subset_of_overlapping_intervals
  output[["interval_selector"]] <- interval_selector
  output[["common_interval"]] <- common_interval
  
  output
}


# # example 1
# 
# lower_bounds <- rnorm(n = 50)
# upper_bouds <- lower_bounds + runif(n = 50)
# 
# intervals <- cbind(lower_bounds, upper_bouds)
# 
# result <- extract_largest_subset_of_overlapping_intervals(table_of_intervals = intervals)
# 
# names(result)
# 
# # largest subset of overlapping intervals
# result$largest_subset_of_overlapping_intervals
# 
# # selector of interval belonging to the largest subset of overlapping intervals
# result$interval_selector
# 
# # common interval to all intervals from the largest subset of overlapping intervals
# result$common_interval
# 
# 
# # example 2
# 
# lower_bounds <- rnorm(n = 100)
# upper_bouds <- lower_bounds + runif(n = 100)
# 
# intervals <- cbind(lower_bounds, upper_bouds)
# 
# result <- extract_largest_subset_of_overlapping_intervals(table_of_intervals = intervals)
# 
# names(result)
# 
# # largest subset of overlapping intervals
# result$largest_subset_of_overlapping_intervals
# 
# # selector of interval belonging to the largest subset of overlapping intervals
# result$interval_selector
# 
# # common interval to all intervals from the largest subset of overlapping intervals
# result$common_interval
# 
# 
# # example 3
# 
# lower_bounds <- rnorm(n = 1000)
# upper_bouds <- lower_bounds + runif(n = 1000)
# 
# intervals <- cbind(lower_bounds, upper_bouds)
# 
# result <- extract_largest_subset_of_overlapping_intervals(table_of_intervals = intervals)
# 
# names(result)
# 
# # largest subset of overlapping intervals
# result$largest_subset_of_overlapping_intervals
# 
# # selector of interval belonging to the largest subset of overlapping intervals
# result$interval_selector
# 
# # common interval to all intervals from the largest subset of overlapping intervals
# result$common_interval
