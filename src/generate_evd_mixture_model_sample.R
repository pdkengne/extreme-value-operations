source("./src/calculate_evd_mixture_model_inverse_cdf.R")
source("./src/calculate_evd_inverse_cdf.R")
source("./src/generate_evd_sample.R")

generate_evd_mixture_model_sample <- function(n = 1, 
                                              locations, 
                                              scales, 
                                              shapes, 
                                              weights, 
                                              kind = c("geometric", "arithmetic", "harmonic")[1]){
  # n: number of observations to generate
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic" or "harmonic"
  
  
  if (kind == "arithmetic"){
    output <- sapply(1:n, function(j){
      
      k <- sample(x = 1:length(weights), 
                  size = 1, 
                  replace = FALSE, 
                  prob = weights)
      
      out <- generate_evd_sample(n = 1, 
                                 loc = locations[k], 
                                 scale = scales[k], 
                                 shape = shapes[k])
      
      out
    })
  }
  else if (kind == "geometric"){
    output <- sapply(1:n, function(j){
      out <- sapply(1:length(weights), function(k){
        power_random_variates <- exp(-rexp(n = 1, rate = weights[k]))
        
        while (power_random_variates == 0 | power_random_variates == 1) {
          power_random_variates <- exp(-rexp(n = 1, rate = weights[k]))
        }
        
        random_value <- calculate_evd_inverse_cdf(p = power_random_variates, 
                                                  loc = locations[k], 
                                                  scale = scales[k], 
                                                  shape = shapes[k])
        
        random_value
      })
      
      max(out)
    })
    
  }
  else if (kind == "harmonic"){
    u <- runif(n = n)
    
    output <- calculate_evd_mixture_model_inverse_cdf(p = u, 
                                                      loc = locations, 
                                                      scale = scales, 
                                                      shape = shapes,
                                                      weights = weights,
                                                      iterations = 10,
                                                      kind = kind)
  }
  else{
    stop("Please enter a correct value to the argument 'kind'. Possible values are 'geometric' or 'arithmetic' or 'harmonic'!")
  }
  
  output
}


# # example 1
# 
# source("./src/calculate_evd_mixture_model_pdf.R")
# 
# weights <- c(0.5, 0.5)
# 
# shapes <- c(0.01, 0.1)
# scales <- c(1, 1)
# locations <- c(-2, 4)
# 
# n <- 10000
# 
# results_1 <- generate_evd_mixture_model_sample(n = n,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# #results_1
# 
# pdf_1 <- calculate_evd_mixture_model_pdf(x = sort(results_1),
#                                          locations,
#                                          scales,
#                                          shapes,
#                                          weights,
#                                          kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# #pdf_1
# 
# density_1 <- density(results_1)
# 
# support_density_1 <- density_1$x
# value_density_1 <- density_1$y
# 
# hist(x = results_1,
#      freq = FALSE,
#      ylim = range(c(value_density_1, pdf_1)),
#      ylab = "density",
#      xlab = "support",
#      main = "mixture model density plot")
# 
# lines(x = support_density_1, y = value_density_1, col = 1, lwd = 2)
# lines(x = sort(results_1), y = pdf_1, col = 4, lwd = 2)
# 
# legend("topright", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# results_2 <- generate_evd_mixture_model_sample(n = n,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# #results_2
# 
# pdf_2 <- calculate_evd_mixture_model_pdf(x = sort(results_2),
#                                          locations,
#                                          scales,
#                                          shapes,
#                                          weights,
#                                          kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# #pdf_2
# 
# density_2 <- density(results_2)
# 
# support_density_2 <- density_2$x
# value_density_2 <- density_2$y
# 
# hist(x = results_2,
#      freq = FALSE,
#      ylim = range(c(value_density_2, pdf_2)),
#      ylab = "density",
#      xlab = "support",
#      main = "mixture model density plot")
# 
# lines(x = support_density_2, y = value_density_2, col = 1, lwd = 2)
# lines(x = sort(results_2), y = pdf_2, col = 4, lwd = 2)
# 
# legend("topright", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# results_3 <- generate_evd_mixture_model_sample(n = n,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# #results_3
# 
# pdf_3 <- calculate_evd_mixture_model_pdf(x = sort(results_3),
#                                          locations,
#                                          scales,
#                                          shapes,
#                                          weights,
#                                          kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# #pdf_3
# 
# density_3 <- density(results_3)
# 
# support_density_3 <- density_3$x
# value_density_3 <- density_3$y
# 
# hist(x = results_3,
#      freq = FALSE,
#      ylim = range(c(value_density_3, pdf_3)),
#      ylab = "density",
#      xlab = "support",
#      main = "mixture model density plot")
# 
# lines(x = support_density_3, y = value_density_3, col = 1, lwd = 2)
# lines(x = sort(results_3), y = pdf_3, col = 4, lwd = 2)
# 
# legend("topright", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# 
# plot(x = sort(results_1),
#      y = pdf_1,
#      type = "l",
#      col = 7,
#      lwd = 2,
#      ylim = range(c(pdf_1, pdf_2, pdf_3)),
#      xlim = range(c(results_1, results_2, results_3)),
#      xlab = "support",
#      ylab = "density",
#      main = "mixture model density plot")
# 
# lines(x = sort(results_2), y  = pdf_2, type = "l", col = 6, lwd = 2)
# lines(x = sort(results_3), y  = pdf_3, type = "l", col = 4, lwd = 2)
# 
# legend("topright", legend = c("geometric", "arithmetic", "harmonic"), col = c(6, 7, 4), lty = c(1, 1, 1))
# 
# 
# results <- generate_evd_mixture_model_sample(n = n,
#                                              locations,
#                                              scales,
#                                              shapes,
#                                              weights,
#                                              kind = "merge")
# 
# 
# 
# # example 2
# 
# source("./src/calculate_evd_mixture_model_pdf.R")
# 
# weights <- c(1/3, 1/3, 1/3)
# 
# shapes <- c(0.001, 0.01, 0.1)
# scales <- c(1, 1, 1)
# 
# locations <- c(-2, +4, +10)
# 
# n <- 10000
# 
# results_1 <- generate_evd_mixture_model_sample(n = n,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# #results_1
# 
# pdf_1 <- calculate_evd_mixture_model_pdf(x = sort(results_1),
#                                          locations,
#                                          scales,
#                                          shapes,
#                                          weights,
#                                          kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# #pdf_1
# 
# density_1 <- density(results_1)
# 
# support_density_1 <- density_1$x
# value_density_1 <- density_1$y
# 
# hist(x = results_1,
#      freq = FALSE,
#      ylim = range(c(value_density_1, pdf_1)),
#      ylab = "density",
#      xlab = "support",
#      main = "mixture model density plot")
# 
# lines(x = support_density_1, y = value_density_1, col = 1, lwd = 2)
# lines(x = sort(results_1), y = pdf_1, col = 4, lwd = 2)
# 
# legend("topright", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# results_2 <- generate_evd_mixture_model_sample(n = n,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# #results_2
# 
# pdf_2 <- calculate_evd_mixture_model_pdf(x = sort(results_2),
#                                          locations,
#                                          scales,
#                                          shapes,
#                                          weights,
#                                          kind = c("geometric", "arithmetic", "harmonic")[1])
# 
# #pdf_2
# 
# density_2 <- density(results_2)
# 
# support_density_2 <- density_2$x
# value_density_2 <- density_2$y
# 
# hist(x = results_2,
#      freq = FALSE,
#      ylim = range(c(value_density_2, pdf_2)),
#      ylab = "density",
#      xlab = "support",
#      main = "mixture model density plot")
# 
# lines(x = support_density_2, y = value_density_2, col = 1, lwd = 2)
# lines(x = sort(results_2), y = pdf_2, col = 4, lwd = 2)
# 
# legend("topright", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# results_3 <- generate_evd_mixture_model_sample(n = n,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# #results_3
# 
# pdf_3 <- calculate_evd_mixture_model_pdf(x = sort(results_3),
#                                          locations,
#                                          scales,
#                                          shapes,
#                                          weights,
#                                          kind = c("geometric", "arithmetic", "harmonic")[3])
# 
# #pdf_3
# 
# density_3 <- density(results_3)
# 
# support_density_3 <- density_3$x
# value_density_3 <- density_3$y
# 
# hist(x = results_3,
#      freq = FALSE,
#      ylim = range(c(value_density_3, pdf_3)),
#      ylab = "density",
#      xlab = "support",
#      main = "mixture model density plot")
# 
# lines(x = support_density_3, y = value_density_3, col = 1, lwd = 2)
# lines(x = sort(results_3), y = pdf_3, col = 4, lwd = 2)
# 
# legend("topright", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# 
# plot(x = sort(results_1),
#      y = pdf_1,
#      type = "l",
#      col = 7,
#      lwd = 2,
#      ylim = range(c(pdf_1, pdf_2, pdf_3)),
#      xlim = range(c(results_1, results_2, results_3)),
#      xlab = "support",
#      ylab = "density",
#      main = "mixture model density plot")
# 
# lines(x = sort(results_2), y  = pdf_2, type = "l", col = 6, lwd = 2)
# lines(x = sort(results_3), y  = pdf_3, type = "l", col = 4, lwd = 2)
# 
# legend("topright", legend = c("geometric", "arithmetic", "harmonic"), col = c(6, 7, 4), lty = c(1, 1, 1))
# 
# 
# results <- generate_evd_mixture_model_sample(n = n,
#                                              locations,
#                                              scales,
#                                              shapes,
#                                              weights,
#                                              kind = "merge")

