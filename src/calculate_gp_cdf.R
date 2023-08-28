# library(evd)

calculate_gp_cdf <- function(q, scale = 1, shape = 0, threshold = 0){
  # x: vector of observations
  # scale, shape, threshold: scale, shape and threshold parameters of the considered gp distribution
  
  gp_cdf <- evd::pgpd(q, scale, shape, loc = threshold)
  
  gp_cdf
}


# # example 1
# 
# result <- calculate_gp_cdf(q = 2:4, scale = 0.5, shape = 0.8, threshold = 1)
# 
# result
# 
# 
# # example 2
# 
# result <- calculate_gp_cdf(q = 2:4, scale = 0.5, shape = 0, threshold = 1)
# 
# result
# 
# 
# # example 3
# 
# result <- calculate_gp_cdf(q = 2:4, scale = 0.5, shape = -0.2, threshold = 1)
# 
# result
