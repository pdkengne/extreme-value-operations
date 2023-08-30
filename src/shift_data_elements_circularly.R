shift_data_elements_circularly <- function(x, lag = +1){
  # x: vector of observations
  # lag: a non zero integer specifying the lag gap
  
  # initialization of the new vector
  m <- length(x)
  y <- rep(NA, m)
  
  if (lag > 0){
    # update of the first elements in the new vector
    y[1:(m - lag)] <- tail(x, n = m - lag)
    
    # update of the last elements in the new vector
    y[(m - lag + 1):m] <- head(x, n = lag)
  } 
  else{
    lag <- abs(lag)
    # update of the first elements in the new vector
    y[1:lag] <- tail(x, n = lag)
    
    # update of the last elements in the new vector
    y[(lag + 1):m] <- head(x, n = m - lag)
  }
  
  y
}


# # example 1
# 
# x <- 1:10
# x
# 
# results <- shift_data_elements_circularly(x, lag = +3)
# 
# results
# 
# 
# # example 2
# 
# x <- 1:10
# x
# 
# results <- shift_data_elements_circularly(x, lag = -3)
# 
# results
# 
# 
# # example 3
# 
# set.seed(123)
# 
# x <- rnorm(n = 10)
# x
# 
# results <- shift_data_elements_circularly(x, lag = +3)
# 
# results
# 
# 
# 
# # example 4
# 
# set.seed(123)
# 
# x <- rnorm(n = 10)
# x
# 
# results <- shift_data_elements_circularly(x, lag = -3)
# 
# results
