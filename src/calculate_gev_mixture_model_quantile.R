source("./src/estimate_gev_parameters.R")

calculate_gev_mixture_model_quantile <- function(gev_mixture_model, 
                                            type = NULL,
                                            model_wise = FALSE,
                                            zoom = FALSE,
                                            xlab = "Theoretical Quantile", 
                                            ylab = "Empirical Quantile", 
                                            main = "Quantile Plot"){
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_mixture_model_parameters()"
  # type: type of gev mixture model to consider. It is one of the following elements
  # model_wise: a boolean which indicates whether to use weights on models or on parameters
  #       ("identic_weights_pw", "pessimistic_weights_pw", "automatic_weights_pw",
  #        "identic_weights_mw", "pessimistic_weights_mw", "automatic_weights_mw").
  # zoom: a boolean which indicates whether to focus on large values or not
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract train data
  uvdata <- gev_mixture_model$data_largest
  
  # extract block size
  block_size <- max(gev_mixture_model$block_sizes)
  
  # find the threshold associated with the block_size
  threshold <- find_threshold_associated_with_given_block_size(x = uvdata, block_size = block_size)
  
  # set the types of weighted gev models
  weighted_gev_model_types = c("identic_weights", "pessimistic_weights", "automatic_weights")
  
  # extract the model parameters (pw)
  gev_model_parameters <- gev_mixture_model$weighted_normalized_gev_parameters_object
  
  # extract the model parameters (mw)
  gev_mixture_model_parameters <- gev_mixture_model$normalized_gev_parameters_object
  gev_mixture_model_parameters_object <- data.frame(cbind(gev_mixture_model_parameters$loc_star,
                                                          gev_mixture_model_parameters$scale_star,
                                                          gev_mixture_model_parameters$shape_star))
  names(gev_mixture_model_parameters_object) <- c("loc_star", "scale_star", "shape_star")
  
  # extract the weight parameters (mw)
  gev_mixture_model_weights_object <- data.frame(cbind(gev_mixture_model$identic_weights_mw,
                                                       gev_mixture_model$pessimistic_weights_mw,
                                                       gev_mixture_model$automatic_weights_mw))
  names(gev_mixture_model_weights_object) <- weighted_gev_model_types
  
  # plot densities
  if (is.element(el = type, set = weighted_gev_model_types) & !model_wise){
    plot_normalized_gev_quantile(x = uvdata, 
                                 loc = gev_model_parameters[type, "loc_star"], 
                                 scale = gev_model_parameters[type, "scale_star"], 
                                 shape = gev_model_parameters[type, "shape_star"], 
                                 zoom = zoom,
                                 threshold = threshold,
                                 xlab = xlab, 
                                 ylab = ylab, 
                                 main = paste(main, ":", type, "- model_wise =", model_wise))
  }
  
  if (is.element(el = type, set = weighted_gev_model_types) & model_wise){
    plot_normalized_gev_mixture_model_quantile(x = uvdata, 
                                               locations = gev_mixture_model_parameters_object$loc_star, 
                                               scales = gev_mixture_model_parameters_object$scale_star, 
                                               shapes = gev_mixture_model_parameters_object$shape_star, 
                                               weights = gev_mixture_model_weights_object[, type],
                                               zoom = zoom,
                                               threshold = threshold,
                                               xlab = xlab, 
                                               ylab = ylab, 
                                               main = paste(main, ":", type, "- model_wise =", model_wise))
  }
  
}



# example 1

source("./src/generate_gev_sample.R")
source("./src/estimate_gev_mixture_model_parameters.R")

n <- 10000
nlargest <- 1000

# x <- rnorm(n = n)
x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.1)

gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
                                                           nsloc = NULL,
                                                           std.err = FALSE,
                                                           block_sizes = NULL,
                                                           minimum_nblocks = 50,
                                                           nlargest = nlargest,
                                                           confidence_level = 0.95,
                                                           trace = TRUE)

# set the types of weighted gev models
weighted_gev_model_types = c("identic_weights", "pessimistic_weights", "automatic_weights")

