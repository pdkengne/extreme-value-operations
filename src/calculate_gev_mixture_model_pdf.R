source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_cdf.R")
source("./src/calculate_gev_mixture_model_cdf.R")

calculate_gev_mixture_model_pdf <- function(x, 
                                            locations, 
                                            scales, 
                                            shapes, 
                                            weights,
                                            kind = c("geometric", "arithmetic")[1]){
  # x: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    S <- sapply(1:length(weights), function(j){
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
  }
  else if (kind == "arithmetic"){
    S <- sapply(1:length(weights), function(j){
      dens <- calculate_gev_pdf(x = x, 
                                loc = locations[j], 
                                scale = scales[j], 
                                shape = shapes[j])
      
      out <- weights[j]*dens
      
      out
    })
    
    if (length(x) == 1){
      output <- sum(S)
    }
    else{
      output <- apply(S, 1, sum)
    }
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
# x <- 1
# 
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = "arith")
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
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[1])
# 
# #results
# 
# plot(x, results, type = "l")
# 
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[2])
# 
# #results
# 
# lines(x, results, type = "l", col = 4)
# 
# 
# 
# # example 3
# 
# weights <- c(0.5, 0.5)
# 
# shapes <- c(0.1, 0.1)
# scales <- c(1, 1)
# locations <- c(-2, +2)
# 
# x <- seq(from = -5, to = 10, length.out = 500)
# 
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[1])
# 
# #results
# 
# plot(x, results, type = "l")
# 
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[2])
# 
# #results
# 
# lines(x, results, type = "l", col = 4)
# legend("topright", legend = c("geometric", "arithmetic"), col = c(1, 4), lty = c(1, 1))
# 
# 
# # example 4
# 
# weights <- c(0.5, 0.5, 0.5)
# 
# shapes <- c(0.1, 0.1, 0.1)
# scales <- c(1, 1, 1)
# locations <- c(-2, +2, +6)
# 
# x <- seq(from = -5, to = 15, length.out = 500)
# 
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[1])
# 
# #results
# 
# plot(x, results, type = "l")
# 
# results <- calculate_gev_mixture_model_pdf(x = x, 
#                                            locations, 
#                                            scales, 
#                                            shapes, 
#                                            weights,
#                                            kind = c("geometric", "arithmetic")[2])
# 
# #results
# 
# lines(x, results, type = "l", col = 4)
# legend("topright", legend = c("geometric", "arithmetic"), col = c(1, 4), lty = c(1, 1))
