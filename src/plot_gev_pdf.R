# library(EnvStats)

source("./src/estimate_gev_parameters.R")
source("./src/calculate_gev_pdf.R")

plot_gev_pdf <- function(gev_model, xlab = "Quantile", ylab = "Density", main = "Probability Density Function (PDF) Plot"){
  # gev_model: an object associated with a result of the function "estimate_gev_parameters()"
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the model parameters
  gev_model_parameters <- gev_model$estimate
  
  # extract train data
  uvdata <- gev_model$data
  
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
  
  legend(x = "topright", 
         legend = c("Empirical PDF", "Theoretical PDF"),
         lty = c(1, 1),
         col = c(4, 2),
         lwd = 2,
         title = "Legend"
  )
  
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)
# 
# gev_model <- estimate_gev_parameters(x, nsloc = NULL)
# 
# plot_gev_pdf(gev_model)
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# gev_model <- estimate_gev_parameters(x, nsloc = NULL)
# 
# plot_gev_pdf(gev_model)
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)
# 
# gev_model <- estimate_gev_parameters(x, nsloc = NULL)
# 
# plot_gev_pdf(gev_model)
