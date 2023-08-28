# library(extRemes)

calculate_gp_cdf <- function(x, loc = 0, scale = 1, shape = 0, threshold = 0){
  # x: vector of observations
  # loc, scale, shape, threshold: location, scale, shape and threshold parameters of the considered gp distribution
  
  gp_cdf <- extRemes::pevd(x, loc, scale, shape, threshold, type = "GP")
  
  gp_cdf
}


# example 1

result <- calculate_gp_cdf(x = 2:4, loc = 1, scale = 0.5, shape = 0.8, threshold = 1)

result


# example 2

result <- calculate_gp_cdf(x = 2:4, loc = 1, scale = 0.5, shape = 0, threshold = 1)

result


# example 3

result <- calculate_gp_cdf(x = 2:4, loc = 1, scale = 0.5, shape = -0.2, threshold = 1)

result
