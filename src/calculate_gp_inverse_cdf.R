# library(extRemes)

options(digits = 10)

calculate_gp_inverse_cdf <- function(p, scale = 1, shape = 0, threshold = 0){
  # p: vector of probabilities
  # scale, shape, threshold: scale, shape and threshold parameters of the considered gp distribution
  
  gp_inverse_cdf <- extRemes::qevd(p = p, 
                                   threshold = threshold, 
                                   scale = scale, 
                                   shape = shape, 
                                   type = "GP")
  
  gp_inverse_cdf
}



# # example 1
# 
# probabilities <- seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gp_inverse_cdf(p = probabilities, scale = 0.5, shape = 0.8, threshold = 1)
# 
# result
# 
# 
# # example 2
# 
# probabilities <- seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gp_inverse_cdf(p = probabilities, scale = 0.5, shape = 0, threshold = 1)
# 
# result
# 
# 
# # example 3
# 
# probabilities <- seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gp_inverse_cdf(p = probabilities, scale = 0.5, shape = -0.2, threshold = 1)
# 
# result
# 
# 
# # example 4
# 
# source("./src/calculate_gp_cdf.R")
# 
# probabilities <- seq(from = 1, to = 9, by = 1)/10
# probabilities
# 
# result <- calculate_gp_cdf(q = calculate_gp_inverse_cdf(p = probabilities, scale = 0.5, shape = 0.8, threshold = 1),
#                             scale = 0.5, shape = 0.8, threshold = 1)
# 
# result
# 
# 
# # example 5
# 
# source("./src/calculate_gp_cdf.R")
# 
# probabilities <- seq(from = 1, to = 9, by = 1)/10
# probabilities
# 
# result <- calculate_gp_cdf(q = calculate_gp_inverse_cdf(p = probabilities))
# 
# result
