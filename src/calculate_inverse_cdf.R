# library(extRemes)

options(digits = 15)

calculate_gev_inverse_cdf <- function(p, loc = 0, scale = 1, shape = 0){
  # x: vector of probabilities
  # loc, scale, shape: location, scale and shape parameters of the considered gev distribution
  
  gev_inverse_cdf <- extRemes::qevd(p, loc, scale, shape, type = "GEV")
  
  gev_inverse_cdf
}


# # example 1
# 
# probabilities <-seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gev_inverse_cdf(p = probabilities, loc = 1, scale = 0.5, shape = 0.8)
# 
# result
# 
# 
# # example 2
# 
# probabilities <-seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gev_inverse_cdf(p = probabilities, loc = 1, scale = 0.5, shape = 0)
# 
# result
# 
# 
# # example 3
# 
# probabilities <-seq(from = 10^(-8), to = 1 -10^(-8), length.out = 20)
# 
# result <- calculate_gev_inverse_cdf(p = probabilities, loc = 1, scale = 0.5, shape = -0.2)
# 
# result
# 
# 
# # example 4
# 
# source("./src/calculate_gev_cdf.R")
# 
# probabilities <-seq(from = 1, to = 9, by = 1)/10
# 
# result <- calculate_gev_cdf(x = calculate_gev_inverse_cdf(p = probabilities))
# 
# result
# probabilities
# 
# 
# # example 5
# 
# source("./src/calculate_gev_cdf.R")
# 
# probabilities <-seq(from = 1, to = 9, by = 1)/10
# 
# result <- calculate_gev_cdf(x = calculate_gev_inverse_cdf(p = probabilities, loc = 1, scale = 0.5, shape = -0.2),
#                             loc = 1, scale = 0.5, shape = -0.2)
# 
# result
# probabilities
