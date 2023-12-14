# library(actuar)

calculate_llogis_mixture_model_cdf <- function(q, 
                                               scales,
                                               shapes,
                                               weights, 
                                               kind = c("geometric", "arithmetic")[1]){
  # q: vector of observations
  # weights: vector of weights
  # scales, shapes: vectors of scale and shape parameters of the considered llogis distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of llogis mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    output <- sapply(q, function(q) {
      S <- sapply(1:length(shapes), function(j) {
        prob <- actuar::pllogis(q = q,
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
        prob <- actuar::pllogis(q = q, 
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
# shapes <- rexp(n = p)
# scales <- rexp(n = p)
# 
# results <- calculate_llogis_mixture_model_cdf(q = 10,
#                                               scales,
#                                               shapes,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# 
# results <- calculate_llogis_mixture_model_cdf(q = 10,
#                                               scales,
#                                               shapes,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# 
# results <- calculate_llogis_mixture_model_cdf(q = 10,
#                                               scales,
#                                               shapes,
#                                               weights,
#                                               kind = "geom")
# 
# 
# # example 2
# 
# p <- 100
# 
# y <- runif(p)
# weights <- y/sum(y)
# 
# shapes <- rexp(n = p)
# scales <- rexp(n = p)
# 
# 
# results <- calculate_llogis_mixture_model_cdf(q = c(2, 3, 5, 7, 11, 13, 17, 19),
#                                               scales,
#                                               shapes,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# results <- calculate_llogis_mixture_model_cdf(q = c(2, 3, 5, 7, 11, 13, 17, 19),
#                                               scales,
#                                               shapes,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[2])
# 
# results

