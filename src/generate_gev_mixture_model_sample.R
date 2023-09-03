source("./src/calculate_gev_mixture_model_inverse_cdf.R")

generate_gev_mixture_model_sample <- function(n = 1, locations, scales, shapes, weights, ntry = 50, quiet = FALSE){
  # n: vnumber of observations to generate
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # ntry: number of random initial guesses to generate
  # quiet: boolean value which indicates whether messages about convergence success or failure should be suppressed
  
  p <- runif(n)
  
  output <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights, ntry, quiet)
  
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
# n <- 20
# 
# results <- generate_gev_mixture_model_sample(n = n, locations, scales, shapes, weights, ntry = 50, quiet = FALSE)
# 
# results
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
# shapes <- runif(n = m, min = -0.01, max = 0.01)
# scales <- runif(n = m)
# locations <- rnorm(n = m)
# 
# n <- 50
# 
# results <- generate_gev_mixture_model_sample(n = n, locations, scales, shapes, weights, ntry = 50, quiet = TRUE)
# 
# results
# 
# pdf <- calculate_gev_mixture_model_pdf(x = sort(results), locations, scales, shapes, weights)
# 
# #pdf
# 
# plot(sort(results), pdf, type = "l")
