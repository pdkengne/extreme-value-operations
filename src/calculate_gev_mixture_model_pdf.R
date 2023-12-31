source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_cdf.R")
source("./src/calculate_gev_mixture_model_cdf.R")

calculate_gev_mixture_model_pdf <- function(x, 
                                            locations, 
                                            scales, 
                                            shapes, 
                                            weights,
                                            kind = c("geometric", "arithmetic", "harmonic")[1]){
  # x: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic" or "harmonic"
  
  if (kind == "geometric"){
    output <- sapply(x, function(x) {
      cdf <- calculate_gev_mixture_model_cdf(q = x, 
                                             locations = locations, 
                                             scales = scales, 
                                             shapes = shapes, 
                                             weights = weights,
                                             kind = kind)
      
      S <- sapply(1:length(shapes), function(j) {
        prob <- calculate_gev_cdf(q = x, 
                                  loc = locations[j], 
                                  scale = scales[j], 
                                  shape = shapes[j])
        
        prob
      })
      
      D <- sapply(1:length(shapes), function(j) {
        dens <- calculate_gev_pdf(x = x, 
                                  loc = locations[j], 
                                  scale = scales[j], 
                                  shape = shapes[j])
        
        dens
      })
      
      Q <- D/(S)
      
      if (length(Q[is.na(Q)]) > 1){
        g <- 0
      }
      else if (min(S) == 0){
        g <- 0
      }
      else{
        g <- (cdf)*sum(weights*D/(S))
      }
      
      g
    })
  }
  else if (kind == "arithmetic"){
    output <- sapply(x, function(x) {
      D <- sapply(1:length(shapes), function(j) {
        dens <- calculate_gev_pdf(x = x, 
                                  loc = locations[j], 
                                  scale = scales[j], 
                                  shape = shapes[j])
        
        dens
      })
      
      g <- sum(weights*D)
      
      g
    })
  
  }
  else if (kind == "harmonic"){
    output <- sapply(x, function(x) {
      cdf <- calculate_gev_mixture_model_cdf(q = x, 
                                             locations = locations, 
                                             scales = scales, 
                                             shapes = shapes, 
                                             weights = weights,
                                             kind = kind)
      
      S <- sapply(1:length(shapes), function(j) {
        prob <- calculate_gev_cdf(q = x, 
                                  loc = locations[j], 
                                  scale = scales[j], 
                                  shape = shapes[j])
        
        prob
      })
      
      D <- sapply(1:length(shapes), function(j) {
        dens <- calculate_gev_pdf(x = x, 
                                  loc = locations[j], 
                                  scale = scales[j], 
                                  shape = shapes[j])
        
        dens
      })
      
      Q <- D/(S^2)
      
      if (length(Q[is.na(Q)]) > 1){
        g <- 0
      }
      else if (min(S^2) == 0){
        g <- 0
      }
      else{
        g <- (cdf^2)*sum(weights*D/(S^2))
      }
      
      g
      })
      
  }
  else{
    stop("Please enter a correct value to the argument 'kind'. Possible values are 'geometric' or 'arithmetic' or 'harmonic'!")
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
# locations <- rnorm(n = p)
# 
# x <- 1
# 
# results <- calculate_gev_mixture_model_pdf(x = x,
#                                            locations,
#                                            scales,
#                                            shapes,
#                                            weights,
#                                            kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# results
# 
# results <- calculate_gev_mixture_model_pdf(x = x,
#                                            locations,
#                                            scales,
#                                            shapes,
#                                            weights,
#                                            kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# results
# 
# results <- calculate_gev_mixture_model_pdf(x = x,
#                                            locations,
#                                            scales,
#                                            shapes,
#                                            weights,
#                                            kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# results
# 
# results <- calculate_gev_mixture_model_pdf(x = x,
#                                            locations,
#                                            scales,
#                                            shapes,
#                                            weights,
#                                            kind = "arith")
# 
# 
# # example 2
# 
# weights <- c(0.5, 0.5)
# 
# shapes <- c(0.1, 0.1)
# scales <- c(1, 1)
# locations <- c(-2, +2)
# 
# x <- seq(from = -5, to = 10, length.out = 500)
# 
# results_1 <- calculate_gev_mixture_model_pdf(x = x,
#                                            locations,
#                                            scales,
#                                            shapes,
#                                            weights,
#                                            kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# #results_1
# 
# 
# results_2 <- calculate_gev_mixture_model_pdf(x = x,
#                                            locations,
#                                            scales,
#                                            shapes,
#                                            weights,
#                                            kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# #results_2
# 
# results_3 <- calculate_gev_mixture_model_pdf(x = x,
#                                              locations,
#                                              scales,
#                                              shapes,
#                                              weights,
#                                              kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# #results_3
# 
# support <- c(results_1, results_2, results_3)
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
# lines(x, results_3, type = "l", col = 4)
# 
# legend("topright", legend = c("geometric", "arithmetic", "harmonic"), col = c(6, 7, 4), lty = c(1, 1, 1))
# 
# 
# # example 3
# 
# weights <- c(1/3, 1/3, 1/3)
# 
# shapes <- c(0.1, 0.1, 0.1)
# scales <- c(1, 1, 1)
# locations <- c(-2, +2, +6)
# 
# x <- seq(from = -5, to = 15, length.out = 500)
# 
# results_1 <- calculate_gev_mixture_model_pdf(x = x,
#                                            locations,
#                                            scales,
#                                            shapes,
#                                            weights,
#                                            kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# #results_1
# 
# 
# results_2 <- calculate_gev_mixture_model_pdf(x = x,
#                                            locations,
#                                            scales,
#                                            shapes,
#                                            weights,
#                                            kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# #results_2
# 
# results_3 <- calculate_gev_mixture_model_pdf(x = x,
#                                              locations,
#                                              scales,
#                                              shapes,
#                                              weights,
#                                              kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# #results_3
# 
# support <- c(results_1, results_2, results_3)
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
# lines(x, results_3, type = "l", col = 4)
# 
# legend("topright", legend = c("geometric", "arithmetic", "harmonic"), col = c(6, 7, 4), lty = c(1, 1, 1))

