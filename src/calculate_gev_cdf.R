# library(extRemes)

calculate_gev_cdf <- function(q, loc = 0, scale = 1, shape = 0){
  # q: vector of observations
  # loc, scale, shape: location, scale and shape parameters of the considered gev distribution
  
  gev_cdf <- extRemes::pevd(q = q, 
                            loc = loc, 
                            scale = scale, 
                            shape = shape, 
                            type = "GEV")
  
  gev_cdf
}



# # example 1
# 
# result <- calculate_gev_cdf(q = 2:4, loc = 1, scale = 0.5, shape = +0.2)
# 
# result
# 
# 
# # example 2
# 
# result <- calculate_gev_cdf(q = 2:4, loc = 1, scale = 0.5, shape = 0)
# 
# result
# 
# 
# # example 3
# 
# result <- calculate_gev_cdf(q = 2:4, loc = 1, scale = 0.5, shape = -0.2)
# 
# result
#