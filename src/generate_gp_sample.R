# library(evd)

generate_gp_sample <- function(n = 1, scale = 1, shape = 0, loc = 0){
  # n: vnumber of observations to generate
  # scale, shape, loc: scale, shape and loc parameters of the considered gp distribution
  
  gp_sample <- evd::rgpd(n, loc, scale, shape)
  
  gp_sample
}



# # example 1
# 
# result <- generate_gp_sample(n = 10, scale = 0.5, shape = 0.8, loc = 0)
# 
# result
# 
# 
# # example 2
# 
# result <- generate_gp_sample(n = 10, scale = 0.5, shape = 0, loc = 1)
# 
# result
# 
# 
# # example 3
# 
# result <- generate_gp_sample(n = 10, scale = 0.5, shape = -0.2, loc = 1)
# 
# result
