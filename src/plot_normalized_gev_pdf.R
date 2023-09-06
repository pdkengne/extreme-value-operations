# library(EnvStats)

source("./src/calculate_gev_pdf.R")

plot_normalized_gev_pdf <- function(x, 
                                    loc = 0, 
                                    scale = 1, 
                                    shape = 0, 
                                    xlab = "Quantile", 
                                    ylab = "Density", 
                                    main = "Probability Density Function (PDF) Plot"){
  # x: vector of observations (not necessary a sample of block maxima)
  # loc, scale, shape: normalized location, scale and shape parameters of the considered gev distribution
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the model parameters
  gev_model_parameters <- c(loc, scale, shape)
  names(gev_model_parameters) <- c("loc", "scale", "shape")
  
  # extract train data
  uvdata <- x
  
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
       main = main
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
                        xlab = "Quantile", 
                        ylab = "Density", 
                        main = "Probability Density Function (PDF) Plot")


# example 2

source("./src/estimate_single_gev_model.R")

x <- rnorm(n = 10000)

gev_model <- estimate_single_gev_model(x, block_size = 20, nsloc = NULL, std.err = FALSE)

gev_model_parameters <- gev_model$normalized_gev_parameters

plot_normalized_gev_pdf(x, 
                        loc = gev_model_parameters["loc_star"], 
                        scale = gev_model_parameters["scale_star"], 
                        shape = gev_model_parameters["shape_star"], 
                        xlab = "Quantile", 
                        ylab = "Density", 
                        main = "Probability Density Function (PDF) Plot")


