transform_discrete_to_continuous_sample <- function(x){
  # x: vector of integer values
  
  y <- floor(x)[!is.na(x)]
  
  y_values <- sort(unique(y))
  
  y_frequency <- as.numeric(table(y))
  
  continuous_sample <- y
  
  for (i in 1:length(y_values)){
    
    x_subset <- y_values[i] + seq(from = 1, to = y_frequency[i] , by = 1)/(y_frequency[i] + 1)
    positions <- which(y == y_values[i])
    continuous_sample[positions] <- x_subset
    
  }
  
  continuous_sample

}


# # example 1
# 
# x <- rpois(n = 50, lambda = 1)
# 
# result <- transform_discrete_to_continuous_sample(x)
# 
# result
# 
# y <- 1:length(x)
# z<- min(x):(max(result) + 1)
# 
# plot(y, x, type = "h", col = 2, ylim = range(z), xlab = "Index", ylab = "Values", 
#      main = "View discrete values (red points) and continuous values (blue points)")
# 
# points(y, x, col = 2, pch = 16)
# points(y, result, col = 4, pch = 16)
# abline(h = floor(z), lty = "dotted")
# 
# 
# # example 2
# 
# x <- rgeom(n = 50, prob = 0.5)
# 
# result <- transform_discrete_to_continuous_sample(x)
# 
# result
# 
# y <- 1:length(x)
# z<- min(x):(max(result) + 1)
# 
# plot(y, x, type = "h", col = 2, ylim = range(z), xlab = "Index", ylab = "Values", 
#      main = "View discrete values (red points) and continuous values (blue points)")
# 
# points(y, x, col = 2, pch = 16)
# points(y, result, col = 4, pch = 16)
# abline(h = floor(z), lty = "dotted")



