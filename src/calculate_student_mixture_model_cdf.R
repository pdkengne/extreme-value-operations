calculate_student_mixture_model_cdf <- function(q, 
                                                locations, 
                                                scales,
                                                shapes,
                                                weights, 
                                                kind = c("geometric", "arithmetic")[1]){
  # q: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location scale and shape parameters of the considered distributions
  # note: shapes stand for the vector which contains degrees of freedom.
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(weights), function(j) {
        prob <- extraDistr::plst(q = q, 
                                 mu = locations[j], 
                                 sigma = scales[j],
                                 df = shapes[j])
        
        out <- prob^(weights[j])
        
        out
      })
      
      G <- prod(S)
      
      G
    })
  }
  else if (kind == "arithmetic"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(weights), function(j) {
        prob <- extraDistr::plst(q = q, 
                                 mu = locations[j], 
                                 sigma = scales[j],
                                 df = shapes[j])
        
        out <- prob*weights[j]
        
        out
      })
      
      G <- sum(S)
      
      G
    })
  }
  else{
    stop("Please enter a correct value to the argument 'kind'. Possible values are 'geometric' or 'arithmetic'!")
  }
  
  output
}


# # example 1
# 
# # library(extraDistr)
# 
# p <- 3
# 
# weights <- as.numeric(extraDistr::rdirichlet(n = 1, alpha = rep(1, times = p)))
# 
# scales <- rexp(n = p)
# locations <- rnorm(n = p)
# shapes <- rpois(n = p, lambda = 10)
# 
# results <- calculate_student_mixture_model_cdf(q = 1:10,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# 
# results <- calculate_student_mixture_model_cdf(q = 1:10,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# 
# results <- calculate_student_mixture_model_cdf(q = 10,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = "geom")

