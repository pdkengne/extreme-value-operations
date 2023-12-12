source("./src/calculate_nsbeta_mixture_model_cdf.R")
source("./src/dichotomy.R")

calculate_nsbeta_mixture_model_inverse_cdf <- function(p, 
                                                       shapes1, 
                                                       shapes2,
                                                       lowers, 
                                                       uppers,
                                                       weights,
                                                       kind = c("geometric", "arithmetic")[1],
                                                       iterations = 100){
  # p: vector of probabilities
  # weights: vector of weights
  # lowers, uppers: vectors of lower and upper bound
  # shapes1, shapes2: vectors of location and scale parameters of the considered distributions
  # The vectors of parameters shape1st have the same number of elements
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  if (is.element(el = kind, set = c("geometric", "arithmetic"))){
    output <- sapply(p, function(p){
      # define the nonlinear equation to solve
      nle <- function(q){
        f <- -p + calculate_nsbeta_mixture_model_cdf(q = q, 
                                                     shapes1 = shapes1, 
                                                     shapes2 = shapes2, 
                                                     lowers = lowers,
                                                     uppers = uppers,
                                                     weights = weights,
                                                     kind = kind)
        f
      }
      
      # get the positions where weights are different from zero
      position_weights_nonzero <- which(weights > 0)
      
      # extract all parameters for which weights are different from zero
      shapes1 <- shapes1[position_weights_nonzero]
      shapes2 <- shapes2[position_weights_nonzero]
      weights <- weights[position_weights_nonzero]
      
      # calculates some initial guesses for the root of the nonlinear equation to solve
      q_initial_guesses <- sapply(1:length(weights), function(j) extraDistr::qnsbeta(p = p, 
                                                                                     shape1 = shapes1[j], 
                                                                                     shape2 = shapes2[j])) 
      
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
# shapes2 <- c(3, 2, 1)
# shapes1 <- c(1, 2, 3)
# lowers <- rep(0, times = p)
# uppers <- rep(1, times = p)
# 
# p <- seq(from = 0.01, to = 0.09, length.out = 9)
# 
# results <- calculate_nsbeta_mixture_model_inverse_cdf(p = p,
#                                                       shapes1, 
#                                                       shapes2,
#                                                       lowers, 
#                                                       uppers,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# calculate_nsbeta_mixture_model_cdf(q = results,
#                                    shapes1, 
#                                    shapes2,
#                                    lowers, 
#                                    uppers,
#                                    weights,
#                                    kind = c("geometric", "arithmetic")[1])
# p
# 
# 
# results <- calculate_nsbeta_mixture_model_inverse_cdf(p = p,
#                                                       shapes1, 
#                                                       shapes2,
#                                                       lowers, 
#                                                       uppers,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# calculate_nsbeta_mixture_model_cdf(q = results,
#                                    shapes1, 
#                                    shapes2,
#                                    lowers, 
#                                    uppers,
#                                    weights,
#                                    kind = c("geometric", "arithmetic")[2])
# p
# 
# 
# results <- calculate_nsbeta_mixture_model_inverse_cdf(p = p,
#                                                       shapes1, 
#                                                       shapes2,
#                                                       lowers, 
#                                                       uppers,
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
# shapes2 <- c(3, 2, 1)
# shapes1 <- c(1, 2, 3)
# lowers <- rep(0, times = p)
# uppers <- rep(1, times = p)
# 
# p <- seq(from = 0.90, to = 0.99, length.out = 10)
# 
# results <- calculate_nsbeta_mixture_model_inverse_cdf(p = p,
#                                                       shapes1, 
#                                                       shapes2,
#                                                       lowers, 
#                                                       uppers,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# calculate_nsbeta_mixture_model_cdf(q = results,
#                                    shapes1, 
#                                    shapes2,
#                                    lowers, 
#                                    uppers,
#                                    weights,
#                                    kind = c("geometric", "arithmetic")[1])
# p
# 
# 
# results <- calculate_nsbeta_mixture_model_inverse_cdf(p = p,
#                                                       shapes1, 
#                                                       shapes2,
#                                                       lowers, 
#                                                       uppers,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# calculate_nsbeta_mixture_model_cdf(q = results,
#                                    shapes1, 
#                                    shapes2,
#                                    lowers, 
#                                    uppers,
#                                    weights,
#                                    kind = c("geometric", "arithmetic")[2])
# p
# 
# 
# results <- calculate_nsbeta_mixture_model_inverse_cdf(p = p,
#                                                       shapes1, 
#                                                       shapes2,
#                                                       lowers, 
#                                                       uppers,
#                                                       weights,
#                                                       iterations = 100,
#                                                       kind = "mix")


