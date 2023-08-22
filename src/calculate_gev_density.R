# library(extRemes)

calculate_gev_density <- function(x, loc = 0, scale = 1, shape = 0){
  # x: vector of observations
  # loc, scale, shape: location, scale and shape parameters of the considered gev distribution
  
  gev_density <- extRemes::devd(x, loc, scale, shape, type = "GEV")
  
  gev_density
}




# # example 1
# 
# result <- calculate_gev_density(x = 2:4, loc = 1, scale = 0.5, shape = 0.8)
# 
# result
# 
# 
# # example 2
# 
# result <- calculate_gev_density(x = 2:4, loc = 1, scale = 0.5, shape = 0)
# 
# result
# 
# 
# # example 2
# 
# result <- calculate_gev_density(x = 2:4, loc = 1, scale = 0.5, shape = -0.2)
# 
# result

