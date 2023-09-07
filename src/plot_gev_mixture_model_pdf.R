# library(EnvStats)

source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/plot_normalized_gev_pdf.R")
source("./src/plot_normalized_gev_mixture_model_pdf.R")

plot_gev_mixture_model_pdf <- function(gev_mixture_model, 
                                       type = NULL,
                                       model_wise = FALSE,
                                       xlab = "Quantile", 
                                       ylab = "Density", 
                                       main = "Probability Density Function (PDF) Plot"){
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_mixture_model_parameters()"
  # type: type of gev mixture model to consider. It is one of the following elements
  # model_wise: a boolean which indicates whether to use weights on models or on parameters
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
  
  # set the types of weighted gev models
  weighted_gev_model_types = c("identic_weights", "pessimistic_weights", "automatic_weights")
  
  # extract the model parameters (pw)
  gev_model_parameters <- gev_mixture_model$weighted_normalized_gev_parameters_object
  
  # extract the model parameters (mw)
  gev_mixture_model_parameters <- gev_mixture_model$normalized_gev_parameters_object
  gev_mixture_model_parameters_object <- data.frame(cbind(gev_mixture_model_parameters$shape_star,
                                                          gev_mixture_model_parameters$scale_star,
                                                          gev_mixture_model_parameters$loc_star))
  names(gev_mixture_model_parameters_object) <- c("loc_star", "scale_star", "shape_star")
  
  # extract the weight parameters (mw)
  gev_mixture_model_weights_object <- data.frame(cbind(gev_mixture_model$identic_weights_mw,
                                                       gev_mixture_model$pessimistic_weights_mw,
                                                       gev_mixture_model$automatic_weights_mw))
  names(gev_mixture_model_weights_object) <- weighted_gev_model_types
  
  # plot densities
  if (is.element(el = type, set = weighted_gev_model_types) & !model_wise){
    plot_normalized_gev_pdf(x = uvdata, 
                            loc = gev_model_parameters[type, "loc_star"], 
                            scale = gev_model_parameters[type, "scale_star"], 
                            shape = gev_model_parameters[type, "shape_star"], 
                            xlab = xlab, 
                            ylab = ylab, 
                            main = paste(main, ":", type, "- model_wise =", model_wise))
  }
  
  if (is.element(el = type, set = weighted_gev_model_types) & model_wise){
    plot_normalized_gev_pdf(x = uvdata, 
                            loc = gev_model_parameters[type, "loc_star"], 
                            scale = gev_model_parameters[type, "scale_star"], 
                            shape = gev_model_parameters[type, "shape_star"], 
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

plot_gev_mixture_model_pdf(gev_mixture_model, 
                           type = "automatic_weights",
                           xlab = "Quantile", 
                           ylab = "Density", 
                           main = "Probability Density Function (PDF) Plot")


















