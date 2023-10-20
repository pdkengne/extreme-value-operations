# library(extRemes)

generate_gev_sample <- function(n = 1, loc = 0, scale = 1, shape = 0){
  # n: number of observations to generate
  # loc, scale, shape: location, scale and shape parameters of the considered gev distribution
  
  gev_sample <- extRemes::revd(n = n, 
                               loc = loc, 
                               scale = scale, 
                               shape = shape, 
                               type = "GEV")
  
  gev_sample
}


# # example 1
# 
# result <- generate_gev_sample(n = 10, loc = 1, scale = 0.5, shape = +0.2)
# 
# result
# 
# 
# # example 2
# 
# result <- generate_gev_sample(n = 10, loc = 1, scale = 0.5, shape = 0)
# 
# result
# 
# 
# # example 3
# 
# result <- generate_gev_sample(n = 10, loc = 1, scale = 0.5, shape = -0.2)
# 
# result
