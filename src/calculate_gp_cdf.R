# library(extRemes)

calculate_gp_cdf <- function(x, scale = 1, shape = 0, threshold = 0){
  # x: vector of observations
  # loc, scale, shape, threshold: scale, shape and threshold parameters of the considered gp distribution
  
  gp_cdf <- extRemes::pevd(x, scale, shape, threshold, type = "GP")
  
  gp_cdf
}


# example 1

result <- calculate_gp_cdf(x = 2:4, scale = 0.5, shape = 0.8, threshold = 1)

result


# example 2

result <- calculate_gp_cdf(x = 2:4, scale = 0.5, shape = 0.8, threshold = 0)

result
