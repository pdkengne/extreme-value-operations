source("./src/calculate_gp_pdf.R")
source("./src/calculate_gp_cdf.R")
source("./src/calculate_gp_mixture_model_cdf.R")

calculate_gp_mixture_model_pdf <- function(x, 
                                           thresholds, 
                                           scales, 
                                           shapes, 
                                           weights,
                                           kind = c("geometric", "arithmetic")[1]){
  # x: vector of observations
  # weights: vector of weights
  # thresholds, scales, shapes: vectors of threshold, scale and shape parameters of the considered gp distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of gp mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    S <- sapply(1:length(weights), function(j){
      dens <- calculate_gp_pdf(x = x, 
                               threshold = thresholds[j], 
                               scale = scales[j], 
                               shape = shapes[j])
      
      prob <- calculate_gp_cdf(q = x, 
                               threshold = thresholds[j], 
                               scale = scales[j], 
                               shape = shapes[j])
      
      dens[prob == 0] <- 0
      prob[prob == 0] <- 1
      
      out <- weights[j]*dens/prob
      
      out
    })
    
    cdf <- calculate_gp_mixture_model_cdf(q = x, 
                                          thresholds = thresholds, 
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
      dens <- calculate_gp_pdf(x = x, 
                               threshold = thresholds[j], 
                               scale = scales[j], 
                               shape = shapes[j])
      
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
# p <- 10
# 
# y <- runif(p)
# weights <- y/sum(y)
# 
# shapes <- runif(n = p, min = -0.1, max = +0.1)
# scales <- rexp(n = p)
# thresholds <- rep(0, times = p)
# 
# x <- 1
# 
# results <- calculate_gp_mixture_model_pdf(x = x,
#                                           thresholds,
#                                           scales,
#                                           shapes,
#                                           weights,
#                                           kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# results <- calculate_gp_mixture_model_pdf(x = x,
#                                           thresholds,
#                                           scales,
#                                           shapes,
#                                           weights,
#                                           kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# results <- calculate_gp_mixture_model_pdf(x = x,
#                                           thresholds,
#                                           scales,
#                                           shapes,
#                                           weights,
#                                           kind = "arith")
# 
# 
# # example 2
# 
# weights <- c(0.5, 0.5)
# 
# shapes <- c(0.01, 0.1)
# scales <- c(1, 5)
# thresholds <- c(0, 0)
# 
# x <- seq(from = -1, to = 20, length.out = 500)
# 
# results_1 <- calculate_gp_mixture_model_pdf(x = x,
#                                             thresholds,
#                                             scales,
#                                             shapes,
#                                             weights,
#                                             kind = c("geometric", "arithmetic")[1])
# 
# #results
# 
# results_2 <- calculate_gp_mixture_model_pdf(x = x,
#                                             thresholds,
#                                             scales,
#                                             shapes,
#                                             weights,
#                                             kind = c("geometric", "arithmetic")[2])
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
# legend("topright", legend = c("geometric", "arithmetic"), col = c(6, 7), lty = c(1, 1))
# 
# 
# 
# # example 3
# 
# weights <- c(1/3, 1/3, 1/3)
# 
# shapes <- c(0.001, 0.01, 0.1)
# scales <- c(1, 3, 5)
# thresholds <- c(0, 0, 0)
# 
# x <- seq(from = -1, to = 15, length.out = 500)
# 
# results_1 <- calculate_gp_mixture_model_pdf(x = x,
#                                             thresholds,
#                                             scales,
#                                             shapes,
#                                             weights,
#                                             kind = c("geometric", "arithmetic")[1])
# 
# #results_1
# 
# results_2 <- calculate_gp_mixture_model_pdf(x = x,
#                                             thresholds,
#                                             scales,
#                                             shapes,
#                                             weights,
#                                             kind = c("geometric", "arithmetic")[2])
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
# legend("topright", legend = c("geometric", "arithmetic"), col = c(6, 7), lty = c(1, 1))

