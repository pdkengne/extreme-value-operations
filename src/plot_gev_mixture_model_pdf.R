# library(EnvStats)

source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")

plot_gev_mixture_model_pdf <- function(gev_mixture_model, 
                                       type = NULL,
                                       xlab = "Quantile", 
                                       ylab = "Density", 
                                       main = "Probability Density Function (PDF) Plot"){
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_mixture_model_parameters()"
  # type: type of gev mixture model to consider. It is one of the following elements
  #       ("identic_weights_pw", "pessimistic_weights_pw", "automatic_weights_pw",
  #        "identic_weights_mw", "pessimistic_weights_mw", "automatic_weights_mw").
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract train data
  uvdata <- gev_mixture_model$data_largest
  
  # calculate empirical pdf
  empirical_density_object <- EnvStats::epdfPlot(x = uvdata, plot.it = FALSE)
  
  # extract ordered quantiles
  ordered_quantiles <- empirical_density_object$x
  
  # extract empirical pdf
  empirical_pdf <- empirical_density_object$f.x
  
  # set the gev mixture model types
  gev_mixture_model_types = c("identic_weights_pw", "pessimistic_weights_pw", "automatic_weights_pw",
                              "identic_weights_mw", "pessimistic_weights_mw", "automatic_weights_mw")
  
  # extract the model parameters (pw)
  gev_model_parameters <- gev_mixture_model$weighted_normalized_gev_parameters_object
  gev_model_parameters_identic_weights <- gev_model_parameters["identic_weights", ]
  gev_model_parameters_pessimistic_weights <- gev_model_parameters["pessimistic_weights", ]
  gev_model_parameters_automatic_weights <- gev_model_parameters["automatic_weights", ]
  
  # extract the model parameters (mw)
  gev_mixture_model_parameters <- gev_mixture_model$normalized_gev_parameters_object
  gev_mixture_model_parameters_shape <- gev_mixture_model_parameters$shape_star
  gev_mixture_model_parameters_scale <- gev_mixture_model_parameters$scale_star
  gev_mixture_model_parameters_loc <- gev_mixture_model_parameters$loc_star
  
  # extract the weight parameters (mw)
  gev_mixture_model_identic_weights <- gev_mixture_model$identic_weights_mw
  gev_mixture_model_pessimistic_weights <- gev_mixture_model$pessimistic_weights_mw
  gev_mixture_model_automatic_weights <- gev_mixture_model$automatic_weights_mw
  
  # plot densities
  if (is.element(el = "identic_weights_pw", set = gev_mixture_model_types)){
    # calculate theoretical pdf
    theoretical_pdf <- calculate_gev_pdf(x = ordered_quantiles, 
                                         loc = gev_model_parameters_identic_weights["loc_star"], 
                                         scale = gev_model_parameters_identic_weights["scale_star"], 
                                         shape = gev_model_parameters_identic_weights["shape_star"])
    
    
  }
  
  
  
  
  
  
}






















