source("./src/calculate_gev_cdf.R")

calculate_gev_mixture_model_cdf <- function(q, 
                                            locations, 
                                            scales, 
                                            shapes, 
                                            weights, 
                                            kind = c("geometric", "arithmetic")[1]){
  # q: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(shapes), function(j) {
        prob <- calculate_gev_cdf(q = q, 
                                  loc = locations[j], 
                                  scale = scales[j], 
                                  shape = shapes[j])
        
        out <- prob^(weights[j])
        
        out
      })
      
      G <- prod(S)
      
      G
    })
  }
  else if (kind == "arithmetic"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(shapes), function(j) {
        prob <- calculate_gev_cdf(q = q, 
                                  loc = locations[j], 
                                  scale = scales[j], 
                                  shape = shapes[j])
        
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
# shapes <- runif(n = p, min = -0.1, max = +0.1)
# scales <- rexp(n = p)
# locations <- rnorm(n = p)
# 
# results <- calculate_gev_mixture_model_cdf(q = 10, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# 
# results <- calculate_gev_mixture_model_cdf(q = 10, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# 
# results <- calculate_gev_mixture_model_cdf(q = 10, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = "geom")
# 
# 
# # example 2
# 
# p <- 100
# 
# y <- runif(p)
# weights <- y/sum(y)
# 
# shapes <- runif(n = p, min = -0.1, max = +0.1)
# scales <- rexp(n = p)
# locations <- rnorm(n = p)
# 
# results <- calculate_gev_mixture_model_cdf(q = c(2, 3, 5, 7, 11, 13, 17, 19), 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# results <- calculate_gev_mixture_model_cdf(q = c(2, 3, 5, 7, 11, 13, 17, 19), 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[2])
# 
# results
