source("./src/calculate_normal_mixture_model_inverse_cdf.R")

generate_normal_mixture_model_sample <- function(n = 1, 
                                              locations, 
                                              scales, 
                                              weights, 
                                              kind = c("geometric", "arithmetic")[1],
                                              iterations = 50){
  # n: number of observations to generate
  # weights: vector of weights
  # locations, scales: vectors of location and scale parameters of the considered distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of mixture model. Possible values are "geometric" or "arithmetic"
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  
  if (kind == "arithmetic"){
    output <- sapply(1:n, function(j){
      
      k <- sample(x = 1:length(weights), 
                  size = 1, 
                  replace = FALSE, 
                  prob = weights)
      
      out <- rnorm(n = 1, 
                   mean = locations[k], 
                   sd = scales[k])
      
      out
    })
  }
  else if (kind == "geometric"){
    output <- sapply(1:n, function(j){
      p <- runif(n = 1, min = 0, max = 1)
      
      sample <- calculate_normal_mixture_model_inverse_cdf(p = p, 
                                                           locations = locations, 
                                                           scales = scales, 
                                                           weights = weights, 
                                                           kind = kind, 
                                                           iterations = iterations)
      
      sample
    })

  }
  else{
    stop("Please enter a correct value to the argument 'kind'. Possible values are 'geometric' or 'arithmetic'!")
  }
  
  output
}



# # example 1
# 
# source("./src/calculate_normal_mixture_model_pdf.R")
# 
# weights <- c(0.5, 0.5)
# 
# scales <- c(1, 1)
# locations <- c(-2, +2)
# 
# n <- 10000
# 
# results_1 <- generate_normal_mixture_model_sample(n = n,
#                                                   locations,
#                                                   scales,
#                                                   weights,
#                                                   kind = c("geometric", "arithmetic")[2])
# 
# #results_1
# 
# pdf_1 <- calculate_normal_mixture_model_pdf(x = sort(results_1),
#                                             locations,
#                                             scales,
#                                             weights,
#                                             kind = c("geometric", "arithmetic")[2])
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
# legend("topleft", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# results_2 <- generate_normal_mixture_model_sample(n = n,
#                                                   locations,
#                                                   scales,
#                                                   weights,
#                                                   kind = c("geometric", "arithmetic")[1])
# 
# #results_2
# 
# pdf_2 <- calculate_normal_mixture_model_pdf(x = sort(results_2),
#                                             locations,
#                                             scales,
#                                             weights,
#                                             kind = c("geometric", "arithmetic")[1])
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
# legend("topleft", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# plot(x = sort(results_1), 
#      y = pdf_1, 
#      type = "l", 
#      col = 7,
#      lwd = 2,
#      ylim = range(c(pdf_1, pdf_2)), 
#      xlim = range(c(results_1, results_2)),
#      xlab = "support",
#      ylab = "density",
#      main = "mixture model density plot")
# 
# lines(x = sort(results_2), y  = pdf_2, type = "l", col = 6, lwd = 2)
# 
# legend("topleft", legend = c("arithmetic", "geometric"), col = c(7, 6), lty = c(1, 1))
# 
# 
# results <- generate_normal_mixture_model_sample(n = n,
#                                                 locations,
#                                                 scales,
#                                                 weights,
#                                                 kind = "merge")
# 
# 
# 
# # example 2
# 
# source("./src/calculate_normal_mixture_model_pdf.R")
# 
# weights <- c(1/3, 1/3, 1/3)
# 
# scales <- c(1, 1, 1)
# locations <- c(-2, +4, +10)
# 
# n <- 10000
# 
# results_1 <- generate_normal_mixture_model_sample(n = n,
#                                                   locations,
#                                                   scales,
#                                                   weights,
#                                                   kind = c("geometric", "arithmetic")[2])
# 
# #results_1
# 
# pdf_1 <- calculate_normal_mixture_model_pdf(x = sort(results_1),
#                                             locations,
#                                             scales,
#                                             weights,
#                                             kind = c("geometric", "arithmetic")[2])
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
# legend("topleft", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# 
# results_2 <- generate_normal_mixture_model_sample(n = n,
#                                                   locations,
#                                                   scales,
#                                                   weights,
#                                                   kind = c("geometric", "arithmetic")[1])
# 
# #results_2
# 
# pdf_2 <- calculate_normal_mixture_model_pdf(x = sort(results_2),
#                                             locations,
#                                             scales,
#                                             weights,
#                                             kind = c("geometric", "arithmetic")[1])
# 
# #pdf_2
# 
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
# legend("topleft", legend = c("empirical", "theoretical"), col = c(1, 4), lty = c(1, 1))
# 
# 
# 
# plot(x = sort(results_1), 
#      y = pdf_1, 
#      type = "l", 
#      col = 7,
#      lwd = 2,
#      ylim = range(c(pdf_1, pdf_2)), 
#      xlim = range(c(results_1, results_2)),
#      xlab = "support",
#      ylab = "density",
#      main = "mixture model density plot")
# 
# lines(x = sort(results_2), y  = pdf_2, type = "l", col = 6, lwd = 2)
# 
# legend("topleft", legend = c("arithmetic", "geometric"), col = c(7, 6), lty = c(1, 1))
# 
# 
# results <- generate_normal_mixture_model_sample(n = n,
#                                                 locations,
#                                                 scales,
#                                                 weights,
#                                                 kind = "merge")


