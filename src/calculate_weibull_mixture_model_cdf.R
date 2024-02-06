calculate_weibull_mixture_model_cdf <- function(q, 
                                                shapes, 
                                                scales,
                                                weights, 
                                                kind = c("geometric", "arithmetic")[1]){
  # q: vector of observations
  # weights: vector of weights
  # shapes, scales: vectors of shape and scale parameters of the considered distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(weights), function(j) {
        prob <- pweibull(q = q, 
                         shape = shapes[j], 
                         scale = scales[j])
        
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
        prob <- pweibull(q = q, 
                         shape = shapes[j], 
                         scale = scales[j])
        
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
# shapes <- runif(n = p)
# 
# results <- calculate_weibull_mixture_model_cdf(q = 1:10,
#                                               shapes,
#                                               scales,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# 
# results <- calculate_weibull_mixture_model_cdf(q = 1:10,
#                                               shapes,
#                                               scales,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# 
# results <- calculate_weibull_mixture_model_cdf(q = 10,
#                                               shapes,
#                                               scales,
#                                               weights,
#                                               kind = "geom")

