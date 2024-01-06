# library(extRemes)


source("./src/get_ns_gev_model_parameters.R")
source("./src/calculate_power_gev_parameters.R")


get_ns_gev_model_normalized_parameters <- function(ns_gev_model, 
                                                   data = NULL,
                                                   block_size = 1,
                                                   extremal_index = 1){
  # ns_gev_model: an object associated with a result of the function "estimate_ns_gev_parameters()"
  # data: dataframe of covariates for linear modeling of the gev model parameters
  # block_size: size of blocks to consider
  # extremal_index: value of the extremal index to consider
  
  # check if the provided dataset is null
  if (is.null(data)){
    data <- ns_gev_model$cov.data
  }
  
  # calculate the gev model parameters object
  gev_parameters_object <- get_ns_gev_model_parameters(ns_gev_model = ns_gev_model, data = data)
  
  # extract the vector gev model parameters
  locations <- gev_parameters_object$location
  scales <- gev_parameters_object$scale
  shapes <- gev_parameters_object$shape
  
  # calculate the normalization exponent
  exponent <- 1/(block_size*extremal_index)
  
  # calculate the gev model normalized parameters object 
  gev_normalized_parameters_object <- sapply(1:nrow(data), function(k){
    calculate_power_gev_parameters(loc = locations[k], 
                                   scale = scales[k], 
                                   shape = shapes[k], 
                                   exponent = exponent)
  })
  
  gev_normalized_parameters_object <- data.frame(t(gev_normalized_parameters_object))
  names(gev_normalized_parameters_object) <- c("location", "scale", "shape")
  rownames(gev_normalized_parameters_object) <- NULL
  
  gev_normalized_parameters_object
}


# # example 1
# 
# data(PORTw, package = "extRemes")
# 
# data <- PORTw
# 
# x <- PORTw$TMX1
# 
# ns_gev_model <- extRemes::fevd(x, data = data, location.fun=~1, use.phi = FALSE, units="deg C")
# 
# results <- get_ns_gev_model_normalized_parameters(ns_gev_model,
#                                                   data = NULL,
#                                                   block_size = 1,
#                                                   extremal_index = 1)
# 
# results
# 
# # findpars(ns_gev_model)
# 
# 
# # example 2
# 
# data(PORTw, package = "extRemes")
# 
# data <- PORTw
# 
# x <- PORTw$TMX1
# 
# ns_gev_model <- extRemes::fevd(x, data = data, location.fun=~AOindex, units="deg C")
# 
# results <- get_ns_gev_model_normalized_parameters(ns_gev_model,
#                                                   data = data,
#                                                   block_size = 1,
#                                                   extremal_index = 1)
# 
# results
# 
# # findpars(ns_gev_model)
# 
# 
# # example 3
# 
# data(PORTw, package = "extRemes")
# 
# data <- PORTw
# 
# x <- PORTw$TMX1
# 
# ns_gev_model <- extRemes::fevd(x, data = data, scale.fun=~AOindex, use.phi = TRUE, units="deg C")
# 
# results <- get_ns_gev_model_normalized_parameters(ns_gev_model,
#                                                   data = data,
#                                                   block_size = 1,
#                                                   extremal_index = 1)
# 
# results
# 
# # findpars(ns_gev_model)
