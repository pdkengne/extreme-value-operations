# library(EnvStats)

source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/calculate_gev_pdf.R")
source("./src/extract_block_maxima.R")


estimate_gev_mixture_model_pdf <- function(gev_mixture_model,
                                           kind = c("geometric", "arithmetic")[1],
                                           x,
                                           estimator_type = c("automatic_weights_mw", 
                                                              "pessimistic_weights_mw", 
                                                              "identic_weights_mw", 
                                                              "automatic_weights_pw",
                                                              "pessimistic_weights_pw", 
                                                              "identic_weights_pw",
                                                              "empirical")[1]){
  # gev_mixture_model: an object associated with a result of the function 
  #                    "estimate_gev_mixture_model_parameters()" or "predict_gev_mixture_model_parameters()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  # x: vector of observations
  # estimator_type: density estimator to use from the set 
  # c("automatic_weights_mw", "pessimistic_weights_mw", "identic_weights_mw", "automatic_weights_pw",
  #   "pessimistic_weights_pw", "identic_weights_pw", "empirical")
  
  # extract the raw data
  raw_data <- gev_mixture_model$data
  
  # extract the largest train data
  data_largest <- gev_mixture_model$data_largest
  
  # set the types of weighted gev models
  weighted_gev_model_types = c("identic_weights", "pessimistic_weights", "automatic_weights")
  
  # extract the model parameters (pw)
  gev_model_parameters <- gev_mixture_model$weighted_normalized_gev_parameters_object
  
  # get the inclusive status of extremal index in gev models
  use_extremal_index <- gev_mixture_model$use_extremal_index
  
  # extract the model parameters (mw)
  if (use_extremal_index){
    gev_mixture_model_parameters_object <- gev_mixture_model$full_normalized_gev_parameters_object
  }
  else{
    gev_mixture_model_parameters_object <- gev_mixture_model$normalized_gev_parameters_object
  }
  
  # extract the weight parameters (mw)
  gev_mixture_model_weights_object <- data.frame(cbind(gev_mixture_model$identic_weights_mw,
                                                       gev_mixture_model$pessimistic_weights_mw,
                                                       gev_mixture_model$automatic_weights_mw))
  names(gev_mixture_model_weights_object) <- weighted_gev_model_types
  
  # extract the list of all estimated gev models
  gev_models_object <- gev_mixture_model$gev_models_object
  
  # extract the vector of block sizes
  block_sizes <- gev_mixture_model$block_sizes
  
  # extract block maxima associated with the largest block size
  block_maxima_largest <- extract_block_maxima(x = data_largest, 
                                               block_size = max(block_sizes))
  
  # set the observation threshold
  x_threshold <- min(block_maxima_largest)
  
  # calculate the pdf
  if (x > x_threshold){
    if (estimator_type == "identic_weights_pw"){
      output <- calculate_gev_pdf(x = x,
                                  loc = gev_model_parameters["identic_weights", "loc_star"],
                                  scale = gev_model_parameters["identic_weights", "scale_star"], 
                                  shape = gev_model_parameters["identic_weights", "shape_star"])
    }
    else if (estimator_type == "pessimistic_weights_pw"){
      output <- calculate_gev_pdf(x = x,
                                  loc = gev_model_parameters["pessimistic_weights", "loc_star"],
                                  scale = gev_model_parameters["pessimistic_weights", "scale_star"], 
                                  shape = gev_model_parameters["pessimistic_weights", "shape_star"])
    }
    else if (estimator_type == "automatic_weights_pw"){
      output <- calculate_gev_pdf(x = x,
                                  loc = gev_model_parameters["automatic_weights", "loc_star"],
                                  scale = gev_model_parameters["automatic_weights", "scale_star"], 
                                  shape = gev_model_parameters["automatic_weights", "shape_star"])
    }
    else if (estimator_type == "identic_weights_mw"){
      output <- calculate_gev_mixture_model_pdf(x = x, 
                                                locations = gev_mixture_model_parameters_object$loc_star, 
                                                scales = gev_mixture_model_parameters_object$scale_star, 
                                                shapes = gev_mixture_model_parameters_object$shape_star, 
                                                weights = gev_mixture_model_weights_object[, "identic_weights"],
                                                kind = kind)
    }
    else if (estimator_type == "pessimistic_weights_mw"){
      output <- calculate_gev_mixture_model_pdf(x = x, 
                                                locations = gev_mixture_model_parameters_object$loc_star, 
                                                scales = gev_mixture_model_parameters_object$scale_star, 
                                                shapes = gev_mixture_model_parameters_object$shape_star, 
                                                weights = gev_mixture_model_weights_object[, "pessimistic_weights"],
                                                kind = kind)
    }
    else if (estimator_type == "automatic_weights_mw"){
      output <- calculate_gev_mixture_model_pdf(x = x, 
                                                locations = gev_mixture_model_parameters_object$loc_star, 
                                                scales = gev_mixture_model_parameters_object$scale_star, 
                                                shapes = gev_mixture_model_parameters_object$shape_star, 
                                                weights = gev_mixture_model_weights_object[, "automatic_weights"],
                                                kind = kind)
    }
    else{
      output <- EnvStats::demp(x = x, obs = raw_data)
    }
  }
  else{
    output <- EnvStats::demp(x = x, obs = raw_data)
  }
  
  output
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/estimate_gev_mixture_model_parameters.R")
# 
# n <- 100000
# nlargest <- 1000
# 
# x <- rnorm(n = n)
# 
# gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
#                                                            block_sizes = NULL,
#                                                            minimum_nblocks = 50,
#                                                            nlargest = nlargest,
#                                                            confidence_level = 0.95,
#                                                            trace = TRUE)
# 
# gev_mixture_model$normalized_gev_parameters_object
# 
# gev_mixture_model$weighted_normalized_gev_parameters_object
# 
# gev_mixture_model$automatic_weights_mw_statistics
# 
# gev_mixture_model$automatic_weights_pw_statistics
# 
# gev_mixture_model$automatic_weights_mw
# 
# 
# estimator_types <- c("automatic_weights_mw",
#                      "pessimistic_weights_mw",
#                      "identic_weights_mw",
#                      "automatic_weights_pw",
#                      "pessimistic_weights_pw",
#                      "identic_weights_pw",
#                      "empirical")
# 
# 
# range(x)
# 
# q <- 4
# 
# results_mw <- estimate_gev_mixture_model_pdf(gev_mixture_model,
#                                              kind = c("geometric", "arithmetic")[1],
#                                              x = q,
#                                              estimator_type = estimator_types[1])
# 
# results_mw
# 
# results_pw <- estimate_gev_mixture_model_pdf(gev_mixture_model,
#                                              kind = c("geometric", "arithmetic")[1],
#                                              x = q,
#                                              estimator_type = estimator_types[4])
# 
# results_pw

