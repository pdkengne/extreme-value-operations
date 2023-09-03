source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_cdf.R")
source("./src/calculate_gev_mixture_model_cdf.R")

calculate_gev_mixture_model_pdf <- function(x, locations, scales, shapes, weights){
  # x: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  
  S <- sapply(1:length(weights), function(j) {
    dens <- calculate_gev_pdf(x = x, 
                              loc = locations[j], 
                              scale = scales[j], 
                              shape = shapes[j])
    
    prob <- calculate_gev_cdf(q = x, 
                              loc = locations[j], 
                              scale = scales[j], 
                              shape = shapes[j])
    
    dens[prob == 0] <- 0
    prob[prob == 0] <- 1
    
    out <- weights[j]*dens/prob
    
    out
  })
  
  cdf <- calculate_gev_mixture_model_cdf(q = x, 
                                         locations = locations, 
                                         scales = scales, 
                                         shapes = shapes, 
                                         weights = weights)
  
  if (length(x) == 1){
    output <- sum(S)*cdf
  }
  else{
    output <- apply(S, 1, sum)*cdf
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
# shapes <- runif(n = p, min = -0.2, max = +0.2)
# scales <- rexp(n = p)
# locations <- rnorm(n = p)
# 
# x <- 1
# 
# results <- calculate_gev_mixture_model_pdf(x = x, locations, scales, shapes, weights)
# 
# results
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
# x <- seq(from = 1, to = 5, length.out = 500)
# 
# results <- calculate_gev_mixture_model_pdf(x = x, locations, scales, shapes, weights)
# 
# #results
# 
# plot(x, results, type = "l")
