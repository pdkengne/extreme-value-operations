# library(extRemes)

calculate_gev_pdf <- function(x, loc = 0, scale = 1, shape = 0){
  # x: vector of observations
  # loc, scale, shape: location, scale and shape parameters of the considered gev distribution
  
  gev_pdf <- extRemes::devd(x = x, 
                            loc = loc, 
                            scale = scale, 
                            shape = shape, 
                            type = "GEV")
  
  gev_pdf
}



# # example 1
# 
# result <- calculate_gev_pdf(x = 2:4, loc = 1, scale = 0.5, shape = +0.2)
# 
# result
# 
# 
# # example 2
# 
# result <- calculate_gev_pdf(x = 2:4, loc = 1, scale = 0.5, shape = 0)
# 
# result
# 
# 
# # example 3
# 
# result <- calculate_gev_pdf(x = 2:4, loc = 1, scale = 0.5, shape = -0.2)
# 
# result

