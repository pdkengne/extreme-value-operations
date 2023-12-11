source("./src/calculate_student_mixture_model_cdf.R")


calculate_student_mixture_model_pdf <- function(x, 
                                                locations, 
                                                scales,
                                                shapes,
                                                weights,
                                                kind = c("geometric", "arithmetic")[1]){
  # x: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location scale and shape parameters of the considered distributions
  # note: shapes stand for the vector which contains degrees of freedom.
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    S <- sapply(1:length(weights), function(j){
      dens <- extraDistr::dlst(x = x, 
                               mu = locations[j], 
                               sigma = scales[j],
                               df = shapes[j])
      
      prob <- extraDistr::plst(q = x, 
                               mu = locations[j], 
                               sigma = scales[j],
                               df = shapes[j])
      
      dens[prob == 0] <- 0
      prob[prob == 0] <- 1
      
      out <- weights[j]*dens/prob
      
      out
    })
    
    cdf <- calculate_student_mixture_model_cdf(q = x, 
                                               locations = locations, 
                                               scales = scales, 
                                               shapes = shapes,
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
      dens <- extraDistr::dlst(x = x, 
                               mu = locations[j], 
                               sigma = scales[j],
                               df = shapes[j])
      
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
# locations <- rnorm(n = p)
# shapes <- rpois(n = p, lambda = 10)
# 
# x <- 1:10
# 
# results <- calculate_student_mixture_model_pdf(x = x,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# results <- calculate_student_mixture_model_pdf(x = x,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# results <- calculate_student_mixture_model_pdf(x = x,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = "arith")
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
# shapes <- c(10, 5)
# 
# x <- seq(from = -5, to = 7, length.out = 500)
# 
# results_1 <- calculate_student_mixture_model_pdf(x = x,
#                                                  locations,
#                                                  scales,
#                                                  shapes,
#                                                  weights,
#                                                  kind = c("geometric", "arithmetic")[1])
# 
# #results_1
# 
# results_2 <- calculate_student_mixture_model_pdf(x = x,
#                                                  locations,
#                                                  scales,
#                                                  shapes,
#                                                  weights,
#                                                  kind = c("geometric", "arithmetic")[2])
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
# shapes <- c(20, 10, 5)
# 
# x <- seq(from = -7, to = 12, length.out = 500)
# 
# results_1 <- calculate_student_mixture_model_pdf(x = x,
#                                                  locations,
#                                                  scales,
#                                                  shapes,
#                                                  weights,
#                                                  kind = c("geometric", "arithmetic")[1])
# 
# #results_1
# 
# results_2 <- calculate_student_mixture_model_pdf(x = x,
#                                                  locations,
#                                                  scales,
#                                                  shapes,
#                                                  weights,
#                                                  kind = c("geometric", "arithmetic")[2])
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

