source("./src/calculate_weibull_mixture_model_cdf.R")
source("./src/dichotomy.R")

calculate_weibull_mixture_model_inverse_cdf <- function(p, 
                                                       shapes, 
                                                       scales,
                                                       weights, 
                                                       kind = c("geometric", "arithmetic")[1],
                                                       iterations = 100){
  # p: vector of probabilities
  # weights: vector of weights
  # shapes, scales: vectors of shape and scale parameters of the considered distributions
  # The vectors of parameters must have the same number of elements
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  if (is.element(el = kind, set = c("geometric", "arithmetic"))){
    output <- sapply(p, function(p){
      # define the nonlinear equation to solve
      nle <- function(q){
        f <- -p + calculate_weibull_mixture_model_cdf(q = q, 
                                                     shapes = shapes, 
                                                     scales = scales, 
                                                     weights = weights,
                                                     kind = kind)
        f
      }
      
      # get the positions where weights are different from zero
      position_weights_nonzero <- which(weights > 0)
      
      # extract all parameters for which weights are different from zero
      shapes <- shapes[position_weights_nonzero]
      scales <- scales[position_weights_nonzero]
      weights <- weights[position_weights_nonzero]
      
      # calculates some initial guesses for the root of the nonlinear equation to solve
      q_initial_guesses <- sapply(1:length(weights), function(j) qweibull(p = p, 
                                                                       shape = shapes[j], 
                                                                       scale = scales[j])) 
      
      # estimate the root of the nonlinear equation to solve
      answer <- dichotomy(func = nle, a = min(q_initial_guesses), b = max(q_initial_guesses), n = iterations)
      
      answer
    })
  }
  else{
    stop("Please enter a correct value to the argument 'kind'. Possible values are 'geometric' or 'arithmetic'!")
  }
  
  output
}



# # example 1
# 
# m <- 3
# 
# weights <- as.numeric(extraDistr::rdirichlet(n = 1, alpha = rep(1, times = m)))
# 
# scales <- rexp(n = m)
# shapes <- runif(n = m)
# 
# p <- seq(from = 0.01, to = 0.09, length.out = 9)
# 
# results <- calculate_weibull_mixture_model_inverse_cdf(p = p,
#                                                       shapes,
#                                                       scales,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# calculate_weibull_mixture_model_cdf(q = results,
#                                 shapes,
#                                 scales,
#                                 weights,
#                                 kind = c("geometric", "arithmetic")[1])
# p
# 
# 
# results <- calculate_weibull_mixture_model_inverse_cdf(p = p,
#                                                       shapes,
#                                                       scales,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# calculate_weibull_mixture_model_cdf(q = results,
#                                    shapes,
#                                    scales,
#                                    weights,
#                                    kind = c("geometric", "arithmetic")[2])
# p
# 
# 
# results <- calculate_weibull_mixture_model_inverse_cdf(p = p,
#                                                       shapes,
#                                                       scales,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = "mix")
# 
# 
# 
# # example 2
# 
# m <- 3
# 
# weights <- as.numeric(extraDistr::rdirichlet(n = 1, alpha = rep(1, times = m)))
# 
# scales <- rexp(n = m)
# shapes <- runif(n = m)
# 
# p <- seq(from = 0.90, to = 0.99, length.out = 10)
# 
# results <- calculate_weibull_mixture_model_inverse_cdf(p = p,
#                                                       shapes,
#                                                       scales,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# calculate_weibull_mixture_model_cdf(q = results,
#                                    shapes,
#                                    scales,
#                                    weights,
#                                    kind = c("geometric", "arithmetic")[1])
# p
# 
# 
# results <- calculate_weibull_mixture_model_inverse_cdf(p = p,
#                                                       shapes,
#                                                       scales,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# calculate_weibull_mixture_model_cdf(q = results,
#                                    shapes,
#                                    scales,
#                                    weights,
#                                    kind = c("geometric", "arithmetic")[2])
# p
# 
# 
# results <- calculate_weibull_mixture_model_inverse_cdf(p = p,
#                                                       shapes,
#                                                       scales,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = "mix")


