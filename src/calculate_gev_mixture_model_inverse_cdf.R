source(("./src/calculate_gev_inverse_cdf.R"))
source("./src/calculate_gev_mixture_model_cdf.R")
source("./src/dichotomy.R")

calculate_gev_mixture_model_inverse_cdf <- function(p, locations, scales, shapes, weights, iterations = 100){
  # p: vector of probabilities
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  output <- sapply(p, function(p){
    # define the nonlinear equation to solve
    nle <- function(q){
      f <- -p + calculate_gev_mixture_model_cdf(q, locations, scales, shapes, weights)
      f
    }
    
    # get the positions where weights are different from zero
    position_weights_nonzero <- which(weights > 0)
    
    # extract all parameters for which weights are different from zero
    locations <- locations[position_weights_nonzero]
    scales <- scales[position_weights_nonzero]
    shapes <- shapes[position_weights_nonzero]
    weights <- weights[position_weights_nonzero]
    
    # calculates some initial guesses for the root of the nonlinear equation to solve
    q_initial_guesses <- sapply(1:length(weights), function(j) calculate_gev_inverse_cdf(p = p, 
                                                                                         loc = locations[j], 
                                                                                         scale = scales[j], 
                                                                                         shape = shapes[j])) 

    # estimate the root of the nonlinear equation to solve
    answer <- dichotomy(func = nle, a = min(q_initial_guesses), b = max(q_initial_guesses), n = iterations)
    
    answer
  })
  
  output
}


# # example 1
# 
# m <- 10
# 
# y <- runif(m)
# weights <- y/sum(y)
# 
# shapes <- runif(n = m, min = -0.1, max = +0.1)
# scales <- rexp(n = m)
# locations <- rnorm(n = m)
# 
# p <- seq(from = 0.01, to = 0.09, length.out = 9)
# 
# results <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights, iterations = 100)
# 
# results
# 
# calculate_gev_mixture_model_cdf(q = results, locations, scales, shapes, weights)
# p
# 
# 
# # example 2
# 
# m <- 10
# 
# y <- runif(m)
# weights <- y/sum(y)
# 
# shapes <- runif(n = m, min = -0.1, max = +0.1)
# scales <- rexp(n = m)
# locations <- rnorm(n = m)
# 
# p <- seq(from = 0.90, to = 0.99, length.out = 10)
# 
# results <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights, iterations = 100)
# 
# results
# 
# calculate_gev_mixture_model_cdf(q = results, locations, scales, shapes, weights)
# p
