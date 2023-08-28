# library(evd)

calculate_gp_pdf <- function(x, scale = 1, shape = 0, loc = 0){
  # x: vector of observations
  # scale, shape, loc: scale, shape and loc parameters of the considered gp distribution
  
  gp_pdf <- evd::dgpd(x, loc, scale, shape)
  
  gp_pdf
}



# # example 1
# 
# result <- calculate_gp_pdf(x = 2:4, scale = 0.5, shape = 0.8, loc = 1)
# 
# result
# 
# 
# # example 2
# 
# result <- calculate_gp_pdf(x = 2:4, scale = 0.5, shape = 0, loc = 1)
# 
# result
# 
# 
# # example 3
# 
# result <- calculate_gp_pdf(x = 2:4, scale = 0.5, shape = -0.2, loc = 1)
# 
# result
