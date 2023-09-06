# library(EnvStats)

source("./src/calculate_gev_pdf.R")

plot_normalized_gev_pdf <- function(x, 
                                    loc = 0, 
                                    scale = 1, 
                                    shape = 0, 
                                    gev_mixture_model = NULL,
                                    type = "automatic_weights",
                                    xlab = "Quantile", 
                                    ylab = "Density", 
                                    main = "Probability Density Function (PDF) Plot"){
  # x: vector of observations
  # loc, scale, shape: normalized location, scale and shape parameters of the considered gev distribution
  # type: type of weighted gev model parameters to consider. It is one of the following elements
  #       ("identic_weights", "pessimistic_weights", "automatic_weights").
  # gev_mixture_model: an object associated with a result of the function "estimate_gev_mixture_model_parameters()"
  #                    Note that if gev_mixture_model != NULL, the argument x, loc, scale and shape are ignored.
  #                    Note that if gev_mixture_model = NULL, the argument type is ignored.
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # set the types of weighted gev model parameters
  weighted_gev_model_types = c("identic_weights", "pessimistic_weights", "automatic_weights")
  
  # extract the required information
  if (is.null(gev_mixture_model)){
    # set the selected type of weighted gev model parameters
    type <- NULL
    
    # extract the model parameters
    gev_model_parameters <- c(loc, scale, shape)
    names(gev_model_parameters) <- c("loc", "scale", "shape")
    
    # extract train data
    uvdata <- x
  }
  else{
    # extract the model parameters
    gev_model_parameters <- gev_mixture_model$weighted_normalized_gev_parameters_object
    gev_model_parameters_identic_weights <- gev_model_parameters["identic_weights", ]
    gev_model_parameters_pessimistic_weights <- gev_model_parameters["pessimistic_weights", ]
    gev_model_parameters_automatic_weights <- gev_model_parameters["automatic_weights", ]
    
    if (is.element(el = type, set = weighted_gev_model_types)){
      # extract the model parameters
      gev_model_parameters <- c(gev_model_parameters[type, "loc_star"],
                                gev_model_parameters[type, "scale_star"],
                                gev_model_parameters[type, "shape_star"])
      names(gev_model_parameters) <- c("loc", "scale", "shape")
    }
    
    # extract train data
    uvdata <- gev_mixture_model$data_largest
  }
  
  # calculate empirical pdf
  empirical_density_object <- EnvStats::epdfPlot(x = uvdata, plot.it = FALSE)
  
  # extract ordered quantiles
  ordered_quantiles <- empirical_density_object$x
  
  # extract empirical pdf
  empirical_pdf <- empirical_density_object$f.x
  
  # calculate theoretical pdf
  theoretical_pdf <- calculate_gev_pdf(x = ordered_quantiles, 
                                       loc = gev_model_parameters["loc"], 
                                       scale = gev_model_parameters["scale"], 
                                       shape = gev_model_parameters["shape"])
  
  # get the pdf range
  pdf_range <- range(c(theoretical_pdf, empirical_pdf))
  
  # plot densities
  plot(x = ordered_quantiles, 
       y = empirical_pdf, 
       type = "l", 
       ylim = pdf_range,
       col = 4,
       lwd = 2,
       cex.axis = 1,
       cex.lab = 1,
       cex.main = 1,
       xlab = xlab,
       ylab = ylab,
       main = paste(main, ":", type)
  )
  
  lines(ordered_quantiles, theoretical_pdf, col = 2, lwd = 2)
  
  abline(h = 0, lty = "dotted", lwd = 1)
  abline(v = median(uvdata), lty = "dotted", lwd = 1)
  
  legend(x = "topright", 
         legend = c("Empirical PDF", "Theoretical PDF"),
         lty = c(1, 1),
         col = c(4, 2),
         lwd = 2,
         title = "Legend"
  )
  
}



# example 1

source("./src/generate_gev_sample.R")
source("./src/plot_gev_pdf.R")

x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)

gev_model <- estimate_gev_parameters(x, nsloc = NULL)

# plot_gev_pdf(gev_model)

gev_model_parameters <- gev_model$estimate

plot_normalized_gev_pdf(x, 
                        loc = gev_model_parameters["loc"], 
                        scale = gev_model_parameters["scale"], 
                        shape = gev_model_parameters["shape"], 
                        gev_mixture_model = NULL,
                        type = "automatic_weights",
                        xlab = "Quantile", 
                        ylab = "Density", 
                        main = "Probability Density Function (PDF) Plot")


# example 2

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

plot_normalized_gev_pdf(x, 
                        loc = 0, 
                        scale = 1, 
                        shape = 0, 
                        gev_mixture_model = gev_mixture_model,
                        type = "automatic_weights",
                        xlab = "Quantile", 
                        ylab = "Density", 
                        main = "Probability Density Function (PDF) Plot")

plot_normalized_gev_pdf(x, 
                        loc = 0, 
                        scale = 1, 
                        shape = 0, 
                        gev_mixture_model = results,
                        type = "identic_weights",
                        xlab = "Quantile", 
                        ylab = "Density", 
                        main = "Probability Density Function (PDF) Plot")

plot_normalized_gev_pdf(x, 
                        loc = 0, 
                        scale = 1, 
                        shape = 0, 
                        gev_mixture_model = results,
                        type = "pessimistic_weights",
                        xlab = "Quantile", 
                        ylab = "Density", 
                        main = "Probability Density Function (PDF) Plot")

