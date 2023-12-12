source("./src/calculate_nsbeta_mixture_model_cdf.R")


calculate_nsbeta_mixture_model_pdf <- function(x, 
                                               shapes1, 
                                               shapes2,
                                               lowers, 
                                               uppers,
                                               weights,
                                                kind = c("geometric", "arithmetic")[1]){
  # x: vector of observations
  # weights: vector of weights
  # lowers, uppers: vectors of lower and upper bound
  # shapes1, shapes2: vectors of location and scale parameters of the considered distributions
  # The vectors of parameters shape1st have the same number of elements
  # kind: indicates the type of mixture model. Possible values are "geometric" or "arithmetic"
  
  if (kind == "geometric"){
    S <- sapply(1:length(weights), function(j){
      dens <- extraDistr::dnsbeta(x = x, 
                                  shape1 = shapes1[j], 
                                  shape2 = shapes2[j],
                                  min = lowers[j],
                                  max = uppers[j])
      
      prob <- extraDistr::pnsbeta(q = x, 
                                  shape1 = shapes1[j], 
                                  shape2 = shapes2[j],
                                  min = lowers[j],
                                  max = uppers[j])
      
      dens[prob == 0] <- 0
      prob[prob == 0] <- 1
      
      out <- weights[j]*dens/prob
      
      out
    })
    
    cdf <- calculate_nsbeta_mixture_model_cdf(q = x, 
                                              shapes1 = shapes1, 
                                              shapes2 = shapes2, 
                                              lowers = lowers,
                                              uppers = uppers,
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
      dens <- extraDistr::dnsbeta(x = x, 
                                  shape1 = shapes1[j], 
                                  shape2 = shapes2[j],
                                  min = lowers[j],
                                  max = uppers[j])
      
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
# shapes2 <- c(3, 2, 1)
# shapes1 <- c(1, 2, 3)
# lowers <- rep(0, times = p)
# uppers <- rep(1, times = p)
# 
# x <- runif(n = 10)
# 
# results <- calculate_nsbeta_mixture_model_pdf(x = x,
#                                               shapes1, 
#                                               shapes2,
#                                               lowers, 
#                                               uppers,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[1])
# 
# results
# 
# results <- calculate_nsbeta_mixture_model_pdf(x = x,
#                                               shapes1, 
#                                               shapes2,
#                                               lowers, 
#                                               uppers,
#                                               weights,
#                                               kind = c("geometric", "arithmetic")[2])
# 
# results
# 
# results <- calculate_nsbeta_mixture_model_pdf(x = x,
#                                               shapes1, 
#                                               shapes2,
#                                               lowers, 
#                                               uppers,
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
# shapes2 <- c(2, 2)
# shapes1 <- c(5, 2)
# lowers <- rep(0, times = p)
# uppers <- rep(1, times = p)
# 
# x <- sort(runif(n = 500))
# 
# results_1 <- calculate_nsbeta_mixture_model_pdf(x = x,
#                                                 shapes1, 
#                                                 shapes2,
#                                                 lowers, 
#                                                 uppers,
#                                                 weights,
#                                                 kind = c("geometric", "arithmetic")[1])
# 
# #results_1
# 
# results_2 <- calculate_nsbeta_mixture_model_pdf(x = x,
#                                                 shapes1, 
#                                                 shapes2,
#                                                 lowers, 
#                                                 uppers,
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
# 
# 
# # example 3
# 
# p <- 3
# 
# weights <- c(1/3, 1/3, 1/3)
# 
# shapes2 <- c(2, 2, 3)
# shapes1 <- c(5, 2, 1)
# lowers <- rep(0, times = p)
# uppers <- rep(1, times = p)
# 
# x <- sort(runif(n = 500))
# 
# results_1 <- calculate_nsbeta_mixture_model_pdf(x = x,
#                                                 shapes1, 
#                                                 shapes2,
#                                                 lowers, 
#                                                 uppers,
#                                                 weights,
#                                                 kind = c("geometric", "arithmetic")[1])
# 
# #results_1
# 
# results_2 <- calculate_nsbeta_mixture_model_pdf(x = x,
#                                                 shapes1, 
#                                                 shapes2,
#                                                 lowers, 
#                                                 uppers,
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

