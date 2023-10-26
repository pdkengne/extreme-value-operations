calculate_power_ncalculate_power_ns_gev_parameterss-gev_parameters <- function(loc = 0, scale = 1, shape = 0, exponent = 1){
  # loc, scale, shape: location, scale and shape parameters of the base GEV model
  # exponent: the exponent to consider
  # Note: Let G denotes a GEV distribution function. The output is a vector containing the parameters
  #       loc_star, scale_star, shape_star such that [G(.; loc, scale, shape)]^(exponent) = G(.; loc_star, scale_star, shape_star).
  
  if (shape == 0){
    loc_star <- loc + scale*log(exponent)
    parameters_star <- c(loc_star, scale, shape)
  }
  else{
    loc_star <- loc + scale*(exponent^(shape) - 1)/shape
    scale_star <- scale*exponent^(shape)
    parameters_star <- c(loc_star, scale_star, shape)
  }
  
  names(parameters_star) <- c("loc_star", "scale_star", "shape_star")
  
  parameters_star
}


# # example 1
# 
# result <- calculate_power_ncalculate_power_ns_gev_parameterss-gev_parameters(loc = 0, scale = 1, shape = 0, exponent = 1)
# result
# 
# 
# # example 2
# 
# result <- calculate_power_ncalculate_power_ns_gev_parameterss-gev_parameters(loc = 0, scale = 1, shape = 0.2, exponent = 1)
# result
# 
# 
# # example 3
# 
# result <- calculate_power_ncalculate_power_ns_gev_parameterss-gev_parameters(loc = 0, scale = 1, shape = 0.2, exponent = 32)
# result
# 
# 
# # example 4
# 
# result <- calculate_power_ncalculate_power_ns_gev_parameterss-gev_parameters(loc = 0, scale = 1, shape = 0, exponent = exp(1))
# result
# 
# names(result)
# result["loc_star"]
# result["scale_star"]
# result["shape_star"]
