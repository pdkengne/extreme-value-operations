# library(BB)

source(("./src/calculate_gev_inverse_cdf.R"))
source("./src/calculate_gev_mixture_model_cdf.R")

calculate_gev_mixture_model_inverse_cdf <- function(p, locations, scales, shapes, weights){
  # p: vector of probabilities
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  
  output <- sapply(p, function(p){
    # define the nonlinear equation to solve
    nle <- function(q){
      f <- -p + calculate_gev_mixture_model_cdf(q, locations, scales, shapes, weights)
      f
    }
    
    # calculates some initial guesses for the root of the nonlinear equation to solve
    q_initial_guesses <- sapply(1:length(weights), function(j) calculate_gev_inverse_cdf(p = p, 
                                                                                         loc = locations[j], 
                                                                                         scale = scales[j], 
                                                                                         shape = shapes[j]))  
    
    # estimate the root of the nonlinear equation to solve
    q_initial_guess <- sort(q_initial_guesses, decreasing = TRUE)
    answer_object <- BB::BBsolve(par = q_initial_guess[1], fn = nle, control=list(trace=FALSE))
    k <- 2
    while (answer_object$convergence != 0 & k <= length(weights)){
      answer_object <- BB::BBsolve(par = q_initial_guess[k], fn = nle, control=list(trace=FALSE))
      k <- k + 1
    }
    answer <- answer_object$par
    names(answer) <- answer_object$message
    
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
# p <- seq(from = 0.1, to = 0.9, length.out = 9)
# 
# results <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights)
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
# p <- seq(from = 0.1, to = 0.9, length.out = 21)
# 
# results <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights)
# 
# results
# 
# calculate_gev_mixture_model_cdf(q = results, locations, scales, shapes, weights)
# p
