# library(stringr)
# library(extRemes)

get_provided_covariates <- function(ns_gev_model, covariates = NULL){
  # ns_gev_model: an object associated with a result of the function "estimate_ns_gev_parameters()"
  # covariates: a named list whose names match the fitted model parameter names
  
  if (ns_gev_model$const.loc & ns_gev_model$const.scale & ns_gev_model$const.shape){
    stop("Please enter a proper non-stationary model in the argument: ns_gev_model!")
  }
  else{
    if (is.null(covariates)){
      stop("Please enter appropriate information in the argument: covariates!")
    }
    else{
      # create an empty output object
      output <- list()
      
      # extract the data frame of all covariates
      data <- ns_gev_model$cov.data
      
      # get the names of useful covariates in each parameter
      gev_model_covariates_list <- ns_gev_model$par.models$term.names
      
      # get the names of useful covariates in the model
      gev_model_covariates_vector <- unique(unlist(gev_model_covariates_list))
      
      # extract the data frame of useful covariates in the model
      used_gev_model_covariates <- data[, gev_model_covariates_vector]

      # get the vector of provided parameter coefficients
      qcov <- extRemes::make.qcov(x = ns_gev_model, vals = covariates)
      covariates_vector <- qcov[1, ]
      
      # get the names of gev model parameters
      covariates_vector_names <- names(covariates_vector)
      
      # get the names of parameters associated with the location
      covariate_location_positions <- stringr::str_detect(string = covariates_vector_names, 
                                                          pattern  = "mu", 
                                                          negate = FALSE)
      covariate_location_names <- covariates_vector_names[covariate_location_positions]
      
      # get the names of parameters associated with the scale
      covariate_scale_positions_1 <- stringr::str_detect(string = covariates_vector_names, 
                                                         pattern  = "sigma", 
                                                         negate = FALSE)
      covariate_scale_positions_2 <- stringr::str_detect(string = covariates_vector_names, 
                                                         pattern  = "phi", 
                                                         negate = FALSE)
      covariate_scale_positions <- covariate_scale_positions_1 | covariate_scale_positions_2
      covariate_scale_names <- covariates_vector_names[covariate_scale_positions]
      
      # get the names of parameters associated with the shape
      covariate_shape_positions <- stringr::str_detect(string = covariates_vector_names, 
                                                       pattern  = "xi", 
                                                       negate = FALSE)
      covariate_shape_names <- covariates_vector_names[covariate_shape_positions]

      # collect the provided covariates
      provided_gev_model_covariates <- used_gev_model_covariates[1, ]
      provided_gev_model_covariates[, gev_model_covariates_list$location] <- covariates_vector[covariate_location_names][-1]
      provided_gev_model_covariates[, gev_model_covariates_list$scale] <- covariates_vector[covariate_scale_names][-1]
      provided_gev_model_covariates[, gev_model_covariates_list$shape] <- covariates_vector[covariate_shape_names][-1]
      
      # update the output object
      output[["used_gev_model_covariates"]] <- used_gev_model_covariates
      output[["provided_gev_model_covariates"]] <- provided_gev_model_covariates
      
      output
    }
    
  }
  
}


# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_ns_gev_parameters.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = +0.2)
# 
# ns_gev_model <- estimate_ns_gev_parameters(x,
#                                            data = NULL,
#                                            location.fun = ~1,
#                                            scale.fun = ~1,
#                                            shape.fun = ~1,
#                                            use.phi = TRUE,
#                                            type = c("GEV", "Gumbel")[1],
#                                            method = c("MLE", "GMLE")[1])
# 
# ns_gev_model
# 
# results <- get_provided_covariates(ns_gev_model = ns_gev_model, covariates = NULL)
# 
# results
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_ns_gev_parameters.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# ns_gev_model <- estimate_ns_gev_parameters(x,
#                                       data = data,
#                                       location.fun = ~ trend + random,
#                                       scale.fun = ~ trend,
#                                       shape.fun = ~ random,
#                                       use.phi = TRUE,
#                                       type = c("GEV", "Gumbel")[1],
#                                       method = c("MLE", "GMLE")[1])
# ns_gev_model
# 
# results <- get_provided_covariates(ns_gev_model = ns_gev_model, covariates = NULL)
# 
# results
# 
# covariates <- list(mu0 = 1,
#                    mu1 = 0.025,
#                    mu2 = 0.25,
#                    phi0 = 1,
#                    phi1 = 0.025,
#                    xi0 = 1,
#                    xi1 = 0.25)
# covariates
# 
# results <- get_provided_covariates(ns_gev_model = ns_gev_model, covariates = covariates)
# 
# names(results)
# 
# head(results$used_gev_model_covariates)
# 
# results$provided_gev_model_covariates
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_ns_gev_parameters.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# ns_gev_model <- estimate_ns_gev_parameters(x,
#                                            data = data,
#                                            location.fun = ~ trend + random,
#                                            scale.fun = ~ trend,
#                                            shape.fun = ~ 1,
#                                            use.phi = TRUE,
#                                            type = c("GEV", "Gumbel")[1],
#                                            method = c("MLE", "GMLE")[1])
# ns_gev_model
# 
# results <- get_provided_covariates(ns_gev_model = ns_gev_model, covariates = NULL)
# 
# results
# 
# covariates <- list(mu0 = 1,
#                    mu1 = 0.025,
#                    mu2 = 0.25,
#                    phi0 = 1,
#                    phi1 = 0.025)
# covariates
# 
# results <- get_provided_covariates(ns_gev_model = ns_gev_model, covariates = covariates)
# 
# names(results)
# 
# head(results$used_gev_model_covariates)
# 
# results$provided_gev_model_covariates
# 
# 
# # example 4
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_ns_gev_parameters.R")
# 
# n <- 1000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# ns_gev_model <- estimate_ns_gev_parameters(x,
#                                            data = data,
#                                            location.fun = ~ trend + random,
#                                            scale.fun = ~ 1,
#                                            shape.fun = ~ 1,
#                                            use.phi = FALSE,
#                                            type = c("GEV", "Gumbel")[1],
#                                            method = c("MLE", "GMLE")[1])
# ns_gev_model
# 
# results <- get_provided_covariates(ns_gev_model = ns_gev_model, covariates = NULL)
# 
# results
# 
# covariates <- list(mu0 = 1,
#                    mu1 = 0.025,
#                    mu2 = 0.25)
# covariates
# 
# qcov <- extRemes::make.qcov(x = ns_gev_model, vals = covariates)
# 
# qcov
# 
# results <- get_provided_covariates(ns_gev_model = ns_gev_model, covariates = covariates)
# 
# names(results)
# 
# head(results$used_gev_model_covariates)
# 
# results$provided_gev_model_covariates
