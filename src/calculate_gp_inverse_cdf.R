# library(evd)

options(digits = 15)

calculate_gp_inverse_cdf <- function(p, scale = 1, shape = 0, loc = 0){
  # p: vector of probabilities
  # scale, shape, loc: scale, shape and loc parameters of the considered gp distribution
  
  gp_inverse_cdf <- evd::qgpd(p, loc, scale, shape)
  
  gp_inverse_cdf
}



# # example 1
# 
# probabilities <- seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gp_inverse_cdf(p = probabilities, scale = 0.5, shape = 0.8, loc = 1)
# 
# result
# 
# 
# # example 2
# 
# probabilities <- seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gp_inverse_cdf(p = probabilities, scale = 0.5, shape = 0, loc = 1)
# 
# result
# 
# 
# # example 3
# 
# probabilities <- seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gp_inverse_cdf(p = probabilities, scale = 0.5, shape = -0.2, loc = 1)
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
# result <- calculate_gp_cdf(q = calculate_gp_inverse_cdf(p = probabilities, scale = 0.5, shape = 0.8, loc = 1),
#                             scale = 0.5, shape = 0.8, loc = 1)
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
