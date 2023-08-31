check_whether_an_element_belongs_to_several_intervals<- function(table_of_intervals, element){
  # function to check if several intervals contain a given value (element).
  # Intervals are specified as a table (table_of_intervals) having two columns.
  # In this table, the first and the second components of each row are respectively the lower and upper bounds of an interval.
  
  output <- table_of_intervals[, 1] <= element & table_of_intervals[, 2] >= element
  
  output
}


# # example 1
# 
# lower_bounds <- rnorm(n = 20)
# upper_bouds <- lower_bounds + runif(n = 20)
# 
# intervals <- cbind(lower_bounds, upper_bouds)
# intervals
# 
# result <- check_whether_an_element_belongs_to_several_intervals(table_of_intervals = intervals, element = -0.5)
# 
# result
# 
# 
# # example 2
# 
# lower_bounds <- rnorm(n = 20)
# upper_bouds <- lower_bounds + runif(n = 20)
# 
# intervals <- cbind(lower_bounds, upper_bouds)
# intervals
# 
# result <- check_whether_an_element_belongs_to_several_intervals(table_of_intervals = intervals, element = +0.5)
# 
# result
# 
# 
# # example 3
# 
# lower_bounds <- rnorm(n = 20)
# upper_bouds <- lower_bounds + runif(n = 20)
# 
# intervals <- cbind(lower_bounds, upper_bouds)
# intervals
# 
# result <- check_whether_an_element_belongs_to_several_intervals(table_of_intervals = intervals, element = 0)
# 
# result
