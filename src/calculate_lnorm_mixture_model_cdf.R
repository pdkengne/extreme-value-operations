
calculate_lnorm_mixture_model_cdf <- function(q, 
                                              locations,
                                              scales,
                                              weights, 
                                              kind = c("geometric", "arithmetic")[1]){
  # q: vector of observations
  # weights: vector of weights
  # locations, scales: vectors of scale and shape parameters of the considered lnorm distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of lnorm mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(scales), function(j) {
        prob <- plnorm(q = q,
                       meanlog = locations[j], 
                       sdlog = scales[j])
        
        out <- prob^(weights[j])
        
        out
      })
      
      G <- prod(S)
      
      G
    })
  }
  else if (kind == "arithmetic"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(scales), function(j) {
        prob <- plnorm(q = q, 
                       meanlog = locations[j], 
                       sdlog = scales[j])
        
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
# p <- 10
# 
# y <- runif(p)
# weights <- y/sum(y)
# 
# scales <- rexp(n = p)
# locations <- rexp(n = p)
# 
# results <- calculate_lnorm_mixture_model_cdf(q = 10,
#                                              locations,
#                                              scales,
#                                              weights,
#                                              kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# 
# results <- calculate_lnorm_mixture_model_cdf(q = 10,
#                                              locations,
#                                              scales,
#                                              weights,
#                                              kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# 
# results <- calculate_lnorm_mixture_model_cdf(q = 10,
#                                              locations,
#                                              scales,
#                                              weights,
#                                              kind = "geom")
# 
# 
# # example 2
# 
# p <- 100
# 
# y <- runif(p)
# weights <- y/sum(y)
# 
# scales <- rexp(n = p)
# locations <- rexp(n = p)
# 
# 
# results <- calculate_lnorm_mixture_model_cdf(q = c(2, 3, 5, 7, 11, 13, 17, 19),
#                                              locations,
#                                              scales,
#                                              weights,
#                                              kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# results <- calculate_lnorm_mixture_model_cdf(q = c(2, 3, 5, 7, 11, 13, 17, 19),
#                                              locations,
#                                              scales,
#                                              weights,
#                                              kind = c("geometric", "arithmetic")[2])
# 
# results

