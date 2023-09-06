extract_nlargest_sample <- function(x, n = Inf){
  # x: vector of observations
  # n: number of largest values 
  #    Note that the vector x is returned, unless n != Inf in which case the vector of n largest values of x is returned.
  
  if (n == Inf){
    nlargest_sample <- x
  }
  else{
    # get the position of the n-largest values in the vector x
    position <- sort(tail(order(x), n))
    
    # extract the n-largest values from the vector x
    nlargest_sample <- x[position]
    names(nlargest_sample) <- position
  }
  
  nlargest_sample
}


# # example 1
# 
# x <- 1:30
# result <- extract_nlargest_sample(x, n = 10)
# result
# x
# 
# 
# # example 2
# 
# x <- rnorm(n = 20)
# result <- extract_nlargest_sample(x, n = 10)
# result
# x
# 
# 
# # example 3
# 
# x <- rnorm(n = 20)
# result <- extract_nlargest_sample(x, n = Inf)
# result
# x
