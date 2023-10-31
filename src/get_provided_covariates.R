# library(stringr)


get_provided_covariates <- function(ns_gev_model, 
                                    data = NULL,
                                    covariates = NULL){
  # ns_gev_model: an object associated with a result of the function "estimate_ns_gev_parameters()"
  # data: dataframe of covariates for linear modeling of the gev model parameters
  # covariates: a named list whose names match the fitted model parameter names
  
  
  
  
  
  
}


library(stringr)
library(extRemes)

data(PORTw)

data <- PORTw

x <- PORTw$TMX1

ns_gev_model <- extRemes::fevd(x, PORTw, location.fun=~ MTMAX + AOindex, scale.fun=~ AOindex + STDMIN, use.phi = FALSE, units="deg C")

ns_gev_model

names(ns_gev_model)

ns_gev_model$const.loc
ns_gev_model$const.scale
ns_gev_model$const.shape

gev_model_covariates_list <- ns_gev_model$par.models$term.names

gev_model_covariates_list

gev_model_covariates_vector <- unique(c(gev_model_covariates_list$location,
                                        gev_model_covariates_list$scale,
                                        gev_model_covariates_list$shape))

gev_model_covariates_vector

gev_model_covariates_df <- data[, gev_model_covariates_vector]

gev_model_provided_covariates_df <- gev_model_covariates_df[1, ]


covariates <- as.list(ns_gev_model$results$par)

covariates_vector <- unlist(covariates)

covariates_vector_names <- names(covariates_vector)

covariate_location_positions <- stringr::str_detect(string = covariates_vector_names, 
                                                    pattern  = "mu", 
                                                    negate = FALSE)


covariate_location_names <- covariates_vector_names[covariate_location_positions]
covariate_location_names

covariate_scale_positions <- stringr::str_detect(string = covariates_vector_names, 
                                                    pattern  = "sigma", 
                                                    negate = FALSE)


covariate_scale_names <- covariates_vector_names[covariate_scale_positions]
covariate_scale_names

covariate_shape_positions <- stringr::str_detect(string = covariates_vector_names, 
                                                 pattern  = "shape", 
                                                 negate = FALSE)


covariate_shape_names <- covariates_vector_names[covariate_shape_positions]
covariate_shape_names


gev_model_provided_covariates_df[, gev_model_covariates_list$location] <- covariates_vector[covariate_location_names][-1]
gev_model_provided_covariates_df[, gev_model_covariates_list$scale] <- covariates_vector[covariate_scale_names][-1]
gev_model_provided_covariates_df[, gev_model_covariates_list$shape] <- covariates_vector[covariate_shape_names][-1]


