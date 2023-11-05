source("./src/calculate_gev_mixture_model_inverse_cdf.R")
source("./src/calculate_power_gev_parameters.R")
source("./src/generate_gev_sample.R")

generate_gev_mixture_model_sample <- function(n = 1, 
                                              locations, 
                                              scales, 
                                              shapes, 
                                              weights, 
                                              kind = c("geometric", "arithmetic")[1],
                                              iterations = 50){
  # n: number of observations to generate
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  if (kind == "arithmetic"){
    output <- sapply(1:n, function(j){
      
      k <- sample(x = 1:length(weights), 
                  size = 1, 
                  replace = FALSE, 
                  prob = weights)
      
      out <- generate_gev_sample(n = 1, 
                                 loc = locations[k], 
                                 scale = scales[k], 
                                 shape = shapes[k])
      
      out
    })
  }
  else if (kind == "geometric"){
    output <- sapply(1:n, function(j){
      out <- sapply(1:length(weights), function(k){
        parameter_star <- calculate_power_gev_parameters(loc = locations[k], 
                                                         scale = scales[k], 
                                                         shape = shapes[k], 
                                                         exponent = weights[k])
        
        random_value <- generate_gev_sample(n = 1, 
                                            loc = parameter_star["loc_star"], 
                                            scale = parameter_star["scale_star"], 
                                            shape = parameter_star["shape_star"])
        
        random_value
      })
      
      max(out)
    })
    
  }
  else{
    stop("Please enter a correct value to the argument 'kind'. Possible values are 'geometric' or 'arithmetic'!")
  }
  
  output
}



# # example 1
# 
# source("./src/calculate_gev_mixture_model_pdf.R")
# 
# weights <- c(0.5, 0.5)
# 
# shapes <- c(0.1, 0.1)
# scales <- c(1, 1)
# locations <- c(-2, +2)
# 
# n <- 1000
# 
# results_1 <- generate_gev_mixture_model_sample(n = n,
#                                              locations,
#                                              scales,
#                                              shapes,
#                                              weights,
#                                              iterations = 50,
#                                              kind = c("geometric", "arithmetic")[2])
# 
# #results_1
# 
# pdf_1 <- calculate_gev_mixture_model_pdf(x = sort(results_1),
#                                        locations,
#                                        scales,
#                                        shapes,
#                                        weights,
#                                        kind = c("geometric", "arithmetic")[2])
# 
# #pdf_1
# 
# results_2 <- generate_gev_mixture_model_sample(n = n,
#                                              locations,
#                                              scales,
#                                              shapes,
#                                              weights,
#                                              iterations = 50,
#                                              kind = c("geometric", "arithmetic")[1])
# 
# #results_2
# 
# pdf_2 <- calculate_gev_mixture_model_pdf(x = sort(results_2),
#                                        locations,
#                                        scales,
#                                        shapes,
#                                        weights,
#                                        kind = c("geometric", "arithmetic")[1])
# 
# #pdf_2
# 
# plot(sort(results_1), pdf_1, type = "l", ylim = range(c(pdf_1, pdf_2)), xlim = range(c(results_1, results_2)))
# lines(sort(results_2), pdf_2, type = "l", col = 4)
# legend("topright", legend = c("arithmetic", "geometric"), col = c(1, 4), lty = c(1, 1))
# 
# results <- generate_gev_mixture_model_sample(n = n,
#                                              locations,
#                                              scales,
#                                              shapes,
#                                              weights,
#                                              iterations = 50,
#                                              kind = "merge")
# 
# 
# 
# # example 2
# 
# source("./src/calculate_gev_mixture_model_pdf.R")
# 
# weights <- c(0.5, 0.5, 0.5)
# 
# shapes <- c(0.1, 0.1, 0.1)
# scales <- c(1, 1, 1)
# locations <- c(-2, +2, +6)
# 
# n <- 1500
# 
# results_1 <- generate_gev_mixture_model_sample(n = n,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                iterations = 50,
#                                                kind = c("geometric", "arithmetic")[2])
# 
# #results_1
# 
# pdf_1 <- calculate_gev_mixture_model_pdf(x = sort(results_1),
#                                          locations,
#                                          scales,
#                                          shapes,
#                                          weights,
#                                          kind = c("geometric", "arithmetic")[2])
# 
# #pdf_1
# 
# results_2 <- generate_gev_mixture_model_sample(n = n,
#                                                locations,
#                                                scales,
#                                                shapes,
#                                                weights,
#                                                iterations = 50,
#                                                kind = c("geometric", "arithmetic")[1])
# 
# #results_2
# 
# pdf_2 <- calculate_gev_mixture_model_pdf(x = sort(results_2),
#                                          locations,
#                                          scales,
#                                          shapes,
#                                          weights,
#                                          kind = c("geometric", "arithmetic")[1])
# 
# #pdf_2
# 
# plot(sort(results_1), pdf_1, type = "l", ylim = range(c(pdf_1, pdf_2)), xlim = range(c(results_1, results_2)))
# lines(sort(results_2), pdf_2, type = "l", col = 4)
# legend("topright", legend = c("arithmetic", "geometric"), col = c(1, 4), lty = c(1, 1))
