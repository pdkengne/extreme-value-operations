source("./eva_pipeline/src/load_functions.R")

load_functions()

calculate_model_aic <- function(x, data, models){
  # x:
  # data:
  # models:
  
  use.phi <- TRUE
  nlargest <- 20000
  block_sizes <- NULL
  minimum_nblocks <- 50
  threshold <- min(x)
  confidence_level <- 0.95
  use_extremal_index <- TRUE
  use_uniform_prior <- TRUE
  method <- "MLE"
  
  fitted_models_object <- lapply(models, function(model){
    try({
      ns_gev_mixture_model_object <- fit_non_stationary_gev_mixture_model(x = x,
                                                                          data = data,
                                                                          location.fun = model$location.fun,
                                                                          scale.fun = model$scale.fun,
                                                                          shape.fun = model$shape.fun,
                                                                          use.phi = use.phi,
                                                                          nlargest = nlargest,
                                                                          block_sizes = block_sizes,
                                                                          minimum_nblocks = minimum_nblocks,
                                                                          threshold = threshold,
                                                                          confidence_level = confidence_level,
                                                                          use_extremal_index = use_extremal_index,
                                                                          use_uniform_prior = use_uniform_prior,
                                                                          method = method)
      
      model_vector <- as.character(model)
      names(model_vector) <- c("location.fun", "scale.fun", "shape.fun")
      
      information_criteria <- ns_gev_mixture_model_object$information_criteria
      
      c(model_vector, information_criteria)
    },
    silent = TRUE)
    
  })
  
  success <- sapply(fitted_models_object, function(x) !inherits(x, "try-error"))
  
  fitted_models_object_success <- fitted_models_object[success]
  
  fitted_models_object_success_df <- do.call(rbind, fitted_models_object_success)
  
  fitted_models_object_success_df <- data.frame("model_names" = rownames(fitted_models_object_success_df),
                                                fitted_models_object_success_df)
  
  rownames(fitted_models_object_success_df) <- 1:nrow(fitted_models_object_success_df)
  
  fitted_models_object_success_df
  
}



