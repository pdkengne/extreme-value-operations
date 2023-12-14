
source("./src/calculate_lnorm_mixture_model_cdf.R")
source("./src/dichotomy.R")

calculate_lnorm_mixture_model_inverse_cdf <- function(p, 
                                                      locations, 
                                                      scales, 
                                                      weights, 
                                                      kind = c("geometric", "arithmetic")[1],
                                                      iterations = 100){
  # p: vector of probabilities
  # weights: vector of weights
  # locations, scales: vectors of scale and shape parameters of the considered lnorm distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of lnorm mixture model. Possible values are "geometric" or "arithmetic"
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  if (is.element(el = kind, set = c("geometric", "arithmetic"))){
    output <- sapply(p, function(p){
      # define the nonlinear equation to solve
      nle <- function(q){
        f <- -p + calculate_lnorm_mixture_model_cdf(q = q, 
                                                    locations = locations, 
                                                    scales = scales, 
                                                    weights = weights,
                                                    kind = kind)
        f
      }
      
      # get the positions where weights are different from zero
      position_weights_nonzero <- which(weights > 0)
      
      # extract all parameters for which weights are different from zero
      locations <- locations[position_weights_nonzero]
      scales <- scales[position_weights_nonzero]
      weights <- weights[position_weights_nonzero]
      
      # calculates some initial guesses for the root of the nonlinear equation to solve
      q_initial_guesses <- sapply(1:length(weights), function(j) qlnorm(p = p, 
                                                                        meanlog = locations[j], 
                                                                        sdlog = scales[j])) 
      
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
# m <- 10
# 
# y <- runif(m)
# weights <- y/sum(y)
# 
# scales <- rexp(n = m)
# locations <- rexp(n = m)
# 
# p <- seq(from = 0.01, to = 0.09, length.out = 9)
# 
# results <- calculate_lnorm_mixture_model_inverse_cdf(p = p,
#                                                      locations,
#                                                      scales,
#                                                      weights,
#                                                      iterations = 100,
#                                                      kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# calculate_lnorm_mixture_model_cdf(q = results,
#                                   locations, scales,
#                                   weights,
#                                   kind = c("geometric", "arithmetic")[1])
# p
# 
# 
# results <- calculate_lnorm_mixture_model_inverse_cdf(p = p,
#                                                      locations,
#                                                      scales,
#                                                      weights,
#                                                      iterations = 100,
#                                                      kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# calculate_lnorm_mixture_model_cdf(q = results,
#                                   locations,
#                                   scales,
#                                   weights,
#                                   kind = c("geometric", "arithmetic")[2])
# p
# 
# 
# results <- calculate_lnorm_mixture_model_inverse_cdf(p = p,
#                                                      locations,
#                                                      scales,
#                                                      weights,
#                                                      iterations = 100,
#                                                      kind = "mix")
# 
# 
# 
# # example 2
# 
# m <- 10
# 
# y <- runif(m)
# weights <- y/sum(y)
# 
# scales <- rexp(n = m)
# locations <- rexp(n = m)
# 
# p <- seq(from = 0.90, to = 0.99, length.out = 10)
# 
# results <- calculate_lnorm_mixture_model_inverse_cdf(p = p,
#                                                      locations,
#                                                      scales,
#                                                      weights,
#                                                      iterations = 100,
#                                                      kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# calculate_lnorm_mixture_model_cdf(q = results,
#                                   locations, scales,
#                                   weights,
#                                   kind = c("geometric", "arithmetic")[1])
# p
# 
# 
# results <- calculate_lnorm_mixture_model_inverse_cdf(p = p,
#                                                      locations,
#                                                      scales,
#                                                      weights,
#                                                      iterations = 100,
#                                                      kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# calculate_lnorm_mixture_model_cdf(q = results,
#                                   locations,
#                                   scales,
#                                   weights,
#                                   kind = c("geometric", "arithmetic")[2])
# p

