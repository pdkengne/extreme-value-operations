source("./src/calculate_logistic_mixture_model_cdf.R")


calculate_logistic_mixture_model_pdf <- function(x, 
                                                 locations, 
                                                 scales, 
                                                 weights,
                                                 kind = c("geometric", "arithmetic")[1]){
  # x: vector of observations
  # weights: vector of weights
  # locations, scales: vectors of location, scale and shape parameters of the considered distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    S <- sapply(1:length(weights), function(j){
      dens <- dlogis(x = x, 
                     location = locations[j], 
                     scale = scales[j])
      
      prob <- plogis(q = x, 
                     location = locations[j], 
                     scale = scales[j])
      
      dens[prob == 0] <- 0
      prob[prob == 0] <- 1
      
      out <- weights[j]*dens/prob
      
      out
    })
    
    cdf <- calculate_logistic_mixture_model_cdf(q = x, 
                                                locations = locations, 
                                                scales = scales, 
                                                weights = weights,
                                                kind = kind)
    
    if (length(x) == 1){
      output <- sum(S)*cdf
    }
    else{
      output <- apply(S, 1, sum)*cdf
    }
  }
  else if (kind == "arithmetic"){
    S <- sapply(1:length(weights), function(j){
      dens <- dlogis(x = x, 
                     location = locations[j], 
                     scale = scales[j])
      
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
# # library(extraDistr)
# 
# p <- 3
# 
# weights <- as.numeric(extraDistr::rdirichlet(n = 1, alpha = rep(1, times = p)))
# 
# scales <- rexp(n = p)
# locations <- rlogis(n = p)
# 
# x <- 1:10
# 
# results <- calculate_logistic_mixture_model_pdf(x = x,
#                                               locations,
#                                               scales,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# results <- calculate_logistic_mixture_model_pdf(x = x,
#                                               locations,
#                                               scales,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# results <- calculate_logistic_mixture_model_pdf(x = x,
#                                               locations,
#                                               scales,
#                                               weights,
#                                               kind = "arith")
# 
# 
# # example 2
# 
# p <- 2
# 
# weights <- c(0.5, 0.5)
# 
# scales <- c(1, 1)
# locations <- c(-2, +2)
# 
# x <- seq(from = -8, to = 8, length.out = 500)
# 
# results_1 <- calculate_logistic_mixture_model_pdf(x = x,
#                                               locations,
#                                               scales,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[1])
# 
# #results_1
# 
# results_2 <- calculate_logistic_mixture_model_pdf(x = x,
#                                               locations,
#                                               scales,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[2])
# 
# #results_2
# 
# support <- c(results_1, results_2)
# 
# plot(x = x,
#      y = results_1,
#      ylim = range(support),
#      type = "l",
#      col = 6,
#      main = "mixture model density plot",
#      xlab = "support",
#      ylab = "density")
# 
# lines(x, results_2, type = "l", col = 7)
# 
# legend("topleft", legend = c("geometric", "arithmetic"), col = c(6, 7), lty = c(1, 1))
# 
# 
# # example 3
# 
# p <- 3
# 
# weights <- c(1/3, 1/3, 1/3)
# 
# scales <- c(1, 1, 1)
# locations <- c(-2, +2, +6)
# 
# x <- seq(from = -10, to = 14, length.out = 500)
# 
# results_1 <- calculate_logistic_mixture_model_pdf(x = x,
#                                                 locations,
#                                                 scales,
#                                                 weights,
#                                                 kind = c("geometric", "arithmetic")[1])
# 
# #results_1
# 
# results_2 <- calculate_logistic_mixture_model_pdf(x = x,
#                                                 locations,
#                                                 scales,
#                                                 weights,
#                                                 kind = c("geometric", "arithmetic")[2])
# 
# #results_2
# 
# support <- c(results_1, results_2)
# 
# plot(x = x,
#      y = results_1,
#      ylim = range(support),
#      type = "l",
#      col = 6,
#      main = "mixture model density plot",
#      xlab = "support",
#      ylab = "density")
# 
# lines(x, results_2, type = "l", col = 7)
# 
# legend("topleft", legend = c("geometric", "arithmetic"), col = c(6, 7), lty = c(1, 1))

