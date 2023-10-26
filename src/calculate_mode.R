# library(DescTools)


calculate_mode <- function(x, data_type = c("continuous", "discrete")[1]){
  # x: a vector of numerical values
  # data_type: indicates the continuous or discrete nature of data in the vector x
  
  if (data_type == "discrete"){
    mode <- DescTools::Mode(x, na.rm = TRUE)
  }
  else{
    mode <- density(x)$x[which.max(density(x)$y)]
  }
  
  as.numeric(mode)
}


# # example 1
# 
# x <- rgeom(n = 100, prob = 0.25)
# 
# results <- calculate_mode(x = x, data_type = c("continuous", "discrete")[2])
# 
# results
# 
# 
# 
# # example 1
# 
# x <- rnorm(n = 1000)
# 
# results <- calculate_mode(x = x, data_type = c("continuous", "discrete")[1])
# 
# results









