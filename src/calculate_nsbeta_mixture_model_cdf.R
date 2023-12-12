calculate_nsbeta_mixture_model_cdf <- function(q, 
                                               shapes1, 
                                               shapes2,
                                               lowers, 
                                               uppers,
                                               weights,
                                               kind = c("geometric", "arithmetic")[1]){
  # q: vector of observations
  # weights: vector of weights
  # lowers, uppers: vectors of lower and upper bound
  # shapes1, shapes2: vectors of location and scale parameters of the considered distributions
  # The vectors of parameters shape1st have the same number of elements
  # kind: indicates the type of mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(weights), function(j) {
        prob <- extraDistr::pnsbeta(q = q, 
                                    shape1 = shapes1[j], 
                                    shape2 = shapes2[j],
                                    min = lowers[j],
                                    max = uppers[j])
        
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
        prob <- extraDistr::pnsbeta(q = q, 
                                    shape1 = shapes1[j], 
                                    shape2 = shapes2[j],
                                    min = lowers[j],
                                    max = uppers[j])
        
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
# shapes2 <- c(3, 2, 1)
# shapes1 <- c(1, 2, 3)
# lowers <- rep(0, times = p)
# uppers <- rep(1, times = p)
# 
# results <- calculate_nsbeta_mixture_model_cdf(q = runif(n = 10),
#                                               shapes1, 
#                                               shapes2,
#                                               lowers, 
#                                               uppers,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# 
# results <- calculate_nsbeta_mixture_model_cdf(q = runif(n = 10),
#                                               shapes1, 
#                                               shapes2,
#                                               lowers, 
#                                               uppers,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# 
# results <- calculate_nsbeta_mixture_model_cdf(q = runif(n = 10),
#                                               shapes1, 
#                                               shapes2,
#                                               lowers, 
#                                               uppers,
#                                               weights,
#                                               kind = "geom")
# 
