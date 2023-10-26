source("./src/calculate_gev_mixture_model_inverse_cdf.R")

generate_gev_mixture_model_sample <- function(n = 1, locations, scales, shapes, weights, iterations = 50){
  # n: number of observations to generate
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  p <- runif(n, min = 0, max = 1)
  
  output <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights, iterations)
  
  output
}


# # example 1
# 
# source("./src/calculate_gev_mixture_model_pdf.R")
# 
# m <- 10
# 
# y <- runif(m)
# weights <- y/sum(y)
# 
# shapes <- runif(n = m, min = -0.01, max = 0.01)
# scales <- runif(n = m)
# locations <- rnorm(n = m)
# 
# n <- 100
# 
# results <- generate_gev_mixture_model_sample(n = n, locations, scales, shapes, weights, iterations = 50)
# 
# #results
# 
# pdf <- calculate_gev_mixture_model_pdf(x = sort(results), locations, scales, shapes, weights)
# 
# #pdf
# 
# plot(sort(results), pdf, type = "l")
# 
# 
# # example 2
# 
# source("./src/calculate_gev_mixture_model_pdf.R")
# 
# m <- 10
# 
# y <- runif(m)
# weights <- y/sum(y)
# 
# shapes <- runif(n = m, min = -0.2, max = -0.1)
# scales <- runif(n = m)
# locations <- rnorm(n = m)
# 
# n <- 1000
# 
# results <- generate_gev_mixture_model_sample(n = n, locations, scales, shapes, weights, iterations = 50)
# 
# #results
# 
# pdf <- calculate_gev_mixture_model_pdf(x = sort(results), locations, scales, shapes, weights)
# 
# #pdf
# 
# plot(sort(results), pdf, type = "l")