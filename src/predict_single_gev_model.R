source("./src/extract_block_maxima_with_indexes.R")
source("./src/predict_gev_parameters.R")
source("./src/calculate_power_gev_parameters.R")


predict_single_gev_model <- function(single_ns_gev_model, 
                                     covariates = NULL,
                                     type = c("GEV", "Gumbel")[1],
                                     method = c("MLE", "GMLE", "Lmoments")[1]){
  # single_ns_gev_model: an object associated with a result of the function "estimate_single_ns_gev_model()"
  # covariates: a named list whose names match the fitted model parameter names
  # type: type of model to use
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  x <- single_ns_gev_model$data
  block_size <- single_ns_gev_model$block_size
  block_maxima_indexes <- single_ns_gev_model$block_maxima_indexes
  extremal_index <- single_ns_gev_model$extremal_index
  ns_gev_model <- single_ns_gev_model$gev_model
  
  if (ns_gev_model$const.loc & ns_gev_model$const.scale & ns_gev_model$const.shape){
    gev_model <- ns_gev_model
    block_maxima <- single_ns_gev_model$block_maxima
  }
  else{
    if (!is.null(covariates)){
      gev_model <- predict_gev_parameters(ns_gev_model = ns_gev_model,
                                          covariates = covariates,
                                          type = type,
                                          method = method)
      block_maxima <- gev_model$x
    }
    else{
      stop("Please enter appropriate information in the argument: covariates!")
    }
  }
  
  # calculate the normalized gev model parameters with block size
  if (method != "Lmoments"){
    gev_parameters <- gev_model$results$par
  }
  else{
    gev_parameters <- gev_model$results
  }
  exponent_bs <- 1/block_size
  normalized_gev_parameters <- calculate_power_gev_parameters(loc = gev_parameters["location"], 
                                                              scale = gev_parameters["scale"], 
                                                              shape = gev_parameters["shape"], 
                                                              exponent = exponent_bs)
  
  # calculate the normalized gev parameters with both block size and extremal index
  exponent_bs_ei <- (1/block_size)*extremal_index
  full_normalized_gev_parameters <- calculate_power_gev_parameters(loc = gev_parameters["location"], 
                                                                   scale = gev_parameters["scale"], 
                                                                   shape = gev_parameters["shape"], 
                                                                   exponent = exponent_bs_ei)
  
  # update the output object
  output[["data"]] <- x
  output[["block_size"]] <- block_size
  output[["block_maxima"]] <- block_maxima
  output[["block_maxima_indexes"]] <- block_maxima_indexes
  output[["extremal_index"]] <- extremal_index
  output[["gev_model"]] <- gev_model
  output[["normalized_gev_parameters"]] <- normalized_gev_parameters
  output[["full_normalized_gev_parameters"]] <- full_normalized_gev_parameters
  
  output
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_single_ns_gev_model.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = +0.2)
# 
# single_ns_gev_model <- estimate_single_ns_gev_model(x,
#                                                     block_size = 1,
#                                                     data = NULL,
#                                                     location.fun = ~1,
#                                                     scale.fun = ~1,
#                                                     shape.fun = ~1,
#                                                     use.phi = TRUE,
#                                                     type = c("GEV", "Gumbel")[1],
#                                                     method = c("MLE", "GMLE")[1])
# 
# # single_ns_gev_model
# names(single_ns_gev_model)
# 
# results <- predict_single_gev_model(single_ns_gev_model = single_ns_gev_model,
#                                     covariates = NULL,
#                                     type = c("GEV", "Gumbel")[1],
#                                     method = c("MLE", "GMLE", "Lmoments")[1])
# 
# #results
# names(results)
# 
# # get the block size
# results$block_size
# 
# # get the extremal index
# results$extremal_index
# 
# # get block maxima
# results$block_maxima
# 
# # get block maxima indexes
# results$block_maxima_indexes
# 
# # get the normalized gev parameters
# results$normalized_gev_parameters
# results$full_normalized_gev_parameters
# 
# # get gev model
# model<- results$gev_model
# 
# model
# 
# names(model)
# 
# # confidence interval for the estimated gev parameters
# extRemes::ci.fevd(model, type = "parameter")
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_single_ns_gev_model.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# single_ns_gev_model <- estimate_single_ns_gev_model(x,
#                                                     block_size = 1,
#                                                     data = data,
#                                                     location.fun = ~ trend,
#                                                     scale.fun = ~ .,
#                                                     shape.fun = ~ random,
#                                                     use.phi = TRUE,
#                                                     type = c("GEV", "Gumbel")[1],
#                                                     method = c("MLE", "GMLE")[1])
# 
# # single_ns_gev_model
# names(single_ns_gev_model)
# 
# ns_gev_model <- single_ns_gev_model$gev_model
# 
# ns_gev_model
# names(ns_gev_model)
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$results$par
# 
# ns_gev_model$par.models$term.names
# 
# names(data)
# 
# covariates <- list(mu1 = 25/n,
#                    phi1 = 25/n,
#                    phi2 = runif(n = 1, min = -0.5, max = 0.5),
#                    xi1 = runif(n = 1, min = -0.5, max = 0.5))
# 
# covariates
# 
# results <- predict_single_gev_model(single_ns_gev_model = single_ns_gev_model,
#                                     covariates = covariates,
#                                     type = c("GEV", "Gumbel")[1],
#                                     method = c("MLE", "GMLE", "Lmoments")[1])
# 
# #results
# names(results)
# 
# # get the block size
# results$block_size
# 
# # get the extremal index
# results$extremal_index
# 
# # get block maxima
# results$block_maxima
# 
# # get block maxima indexes
# results$block_maxima_indexes
# 
# # get the normalized gev parameters
# results$normalized_gev_parameters
# results$full_normalized_gev_parameters
# 
# # get gev model
# model<- results$gev_model
# 
# model
# 
# names(model)
# 
# # confidence interval for the estimated gev parameters
# extRemes::ci.fevd(model, type = "parameter")
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_single_ns_gev_model.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# single_ns_gev_model <- estimate_single_ns_gev_model(x,
#                                                     block_size = 1,
#                                                     data = data,
#                                                     location.fun = ~ trend,
#                                                     scale.fun = ~ .,
#                                                     shape.fun = ~ 1,
#                                                     use.phi = TRUE,
#                                                     type = c("GEV", "Gumbel")[1],
#                                                     method = c("MLE", "GMLE")[1])
# 
# # single_ns_gev_model
# names(single_ns_gev_model)
# 
# ns_gev_model <- single_ns_gev_model$gev_model
# 
# ns_gev_model
# names(ns_gev_model)
# ns_gev_model$const.loc
# ns_gev_model$const.scale
# ns_gev_model$const.shape
# 
# ns_gev_model$results$par
# 
# ns_gev_model$par.models$term.names
# 
# names(data)
# 
# covariates <- list(mu0 = 1,
#                    mu1 = 25/n,
#                    phi0 = 1,
#                    phi1 = 25/n,
#                    phi2 = runif(n = 1, min = -0.5, max = 0.5))
# covariates
# 
# results <- predict_single_gev_model(single_ns_gev_model = single_ns_gev_model,
#                                     covariates = NULL,
#                                     type = c("GEV", "Gumbel")[1],
#                                     method = c("MLE", "GMLE", "Lmoments")[1])
# 
# # results
# names(results)
# 
# results <- predict_single_gev_model(single_ns_gev_model = single_ns_gev_model,
#                                     covariates = covariates,
#                                     type = c("GEV", "Gumbel")[1],
#                                     method = c("MLE", "GMLE", "Lmoments")[1])
# 
# # results
# names(results)
# 
# # get the block size
# results$block_size
# 
# # get the extremal index
# results$extremal_index
# 
# # get block maxima
# results$block_maxima
# 
# # get block maxima indexes
# results$block_maxima_indexes
# 
# # get the normalized gev parameters
# results$normalized_gev_parameters
# results$full_normalized_gev_parameters
# 
# # get gev model
# model<- results$gev_model
# 
# model
# 
# names(model)
# 
# # confidence interval for the estimated gev parameters
# extRemes::ci.fevd(model, type = "parameter")
