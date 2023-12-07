# library(stringr)
# library(extRemes)

library(tidyverse)


calculate_ns_gev_model_parameters <- function(ns_gev_model, data){
  # ns_gev_model: an object associated with a result of the function "extRemes::fevd()"
  # data: dataframe of covariates for linear modeling of the gev model parameters
  
  # create an empty output object
  output <- list()
  
  n <-  nrow(data)
  parameters <- ns_gev_model$results$par
  parameter_names <- names(parameters)
  
  model_parameters <- ns_gev_model$par.models
  model_parameters_variable_names <- model_parameters$term.names
  
  model_location_function <- model_parameters$location
  model_scale_function <- model_parameters$scale
  model_shape_function <- model_parameters$shape

  if (extRemes::is.fixedfevd(x = ns_gev_model)){
    
    location <- rep(as.numeric(parameters["location"]), times = n)
    shape <- rep(as.numeric(parameters["shape"]), times = n)
    
    if (model_parameters$log.scale){
      scale <- rep(exp(as.numeric(parameters["log.scale"])), times = n)
    } else{
      scale <- rep(as.numeric(parameters["scale"]), times = n)
    }
    
  } else{
    
    if (extRemes::check.constant(x = model_location_function)){
      location <- rep(as.numeric(parameters["location"]), times = n)
    } else{
      model_location_variable_names <- model_parameters_variable_names$location
      if (is.element(el = ".", set = model_location_variable_names)){
        model_location_variable_names <- names(data)
      }
      model_location_data <- data %>% select(all_of(model_location_variable_names))
      model_location_data <- t(model_location_data %>% add_column("constant" = 1, .before = 1))
      
      location_parameter_positions <- stringr::str_detect(string = parameter_names, 
                                                          pattern  = "mu", 
                                                          negate = FALSE)
      location_parameters <- parameters[location_parameter_positions]
      
      location <- location_parameters %*% model_location_data
    }
    
    
    if (extRemes::check.constant(x = model_scale_function)){
      scale <- rep(as.numeric(parameters["scale"]), times = n)
    } else{
      model_scale_variable_names <- model_parameters_variable_names$scale
      if (is.element(el = ".", set = model_scale_variable_names)){
        model_scale_variable_names <- names(data)
      }
      model_scale_data <- data %>% select(all_of(model_scale_variable_names))
      model_scale_data <- t(model_scale_data %>% add_column("constant" = 1, .before = 1))
      
      if (model_parameters$log.scale){
        scale_parameter_positions <- stringr::str_detect(string = parameter_names, 
                                                         pattern  = "phi", 
                                                         negate = FALSE)
        scale_parameters <- parameters[scale_parameter_positions]
        
        scale <- exp(scale_parameters %*% model_scale_data)
      } else{
        scale_parameter_positions <- stringr::str_detect(string = parameter_names, 
                                                         pattern  = "sigma", 
                                                         negate = FALSE)
        scale_parameters <- parameters[scale_parameter_positions]
        
        scale <- scale_parameters %*% model_scale_data
      }
    }
    
    
    if (extRemes::check.constant(x = model_shape_function)){
      shape <- rep(as.numeric(parameters["shape"]), times = n)
    } else{
      model_shape_variable_names <- model_parameters_variable_names$shape
      if (is.element(el = ".", set = model_shape_variable_names)){
        model_shape_variable_names <- names(data)
      }
      model_shape_data <- data %>% select(all_of(model_shape_variable_names))
      model_shape_data <- t(model_shape_data %>% add_column("constant" = 1, .before = 1))
      
      shape_parameter_positions <- stringr::str_detect(string = parameter_names, 
                                                       pattern  = "xi", 
                                                       negate = FALSE)
      shape_parameters <- parameters[shape_parameter_positions]
      
      shape <- shape_parameters %*% model_shape_data
    }
    
  }
  
  
  # update the output object
  output[["location"]] <- location
  output[["scale"]] <- scale
  output[["shape"]] <- shape
  
  output
}


# # example 1
# 
# data(PORTw, package = "extRemes")
# 
# data <- PORTw
# 
# x <- PORTw$TMX1
# 
# ns_gev_model <- fevd(x, data, location.fun=~1, use.phi = FALSE, units="deg C")
# 
# results <- calculate_ns_gev_model_parameters(ns_gev_model, data)
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
# ns_gev_model <- fevd(x, data, location.fun=~AOindex, units="deg C")
# 
# results <- calculate_ns_gev_model_parameters(ns_gev_model, data)
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
# ns_gev_model <- fevd(x, data, scale.fun=~AOindex, use.phi = TRUE, units="deg C")
# 
# results <- calculate_ns_gev_model_parameters(ns_gev_model, data)
# 
# results
# 
# # findpars(ns_gev_model)
