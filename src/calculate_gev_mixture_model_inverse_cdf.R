# library(BB)

source(("./src/calculate_gev_inverse_cdf.R"))
source("./src/calculate_gev_mixture_model_cdf.R")

calculate_gev_mixture_model_inverse_cdf <- function(p, locations, scales, shapes, weights, ntry = 50, quiet = FALSE){
  # p: vector of probabilities
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # ntry: number of random initial guesses to generate
  # quiet: boolean value which indicates whether messages about convergence success or failure should be suppressed
  
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
    # generate some initial guesses for the root of the nonlinear equation to solve
    random_q_initial_guesses <- seq(from = min(q_initial_guesses), to = max(q_initial_guesses), length.out = ntry)
    random_q_initial_guesses_matrix <- matrix(data = random_q_initial_guesses, nrow = ntry, ncol = 1)

    # estimate the root of the nonlinear equation to solve
    answer_object <- BB::multiStart(par = random_q_initial_guesses_matrix, 
                                    fn = nle, 
                                    action = "solve", 
                                    quiet = quiet,
                                    control = list(trace=FALSE))
    
    # extract the exact root of the nonlinear equation to solve
    answer_object_converged <- answer_object$par[answer_object$converged, ]
    answer <- ifelse(test = length(answer_object_converged) >= 1,
                     yes = answer_object_converged[1],
                     no = NA)
    
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
# results <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights, ntry = 50, quiet = FALSE)
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
# results <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights, ntry = 50, quiet = TRUE)
# 
# results
# 
# calculate_gev_mixture_model_cdf(q = results, locations, scales, shapes, weights)
# p
