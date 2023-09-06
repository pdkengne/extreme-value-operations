# library(EnvStats)

source("./src/calculate_gev_mixture_model_pdf.R")

plot_gev_mixture_model_pdf <- function(gev_mixture_model, 
                                       type = NULL,
                                       xlab = "Quantile", 
                                       ylab = "Density", 
                                       main = "Probability Density Function (PDF) Plot"){
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_parameters()"
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
  
  
  
  
  # set the gev mixture model types
  gev_mixture_model_types = c("identic_weights_pw", "pessimistic_weights_pw", "automatic_weights_pw",
                              "identic_weights_mw", "pessimistic_weights_mw", "automatic_weights_mw")
  
  
  
  
  
  
  
  
  
}






















