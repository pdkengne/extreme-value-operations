# library(EnvStats)

source("./src/estimate_gp_parameters.R")
source("./src/calculate_gp_pdf.R")

plot_gp_pdf <- function(gp_model, xlab = "Quantile (Excesses)", ylab = "Density", main = "Probability Density Function (PDF) Plot"){
  # gp_model: an object associated with a result of the function "estimate_gp_parameters()"
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the model parameters
  gp_model_parameters <- gp_model$estimate
  
  # extract the used threshold
  threshold <- gp_model$threshold
  
  # extract the exceedances
  exceedances <- gp_model$exceedances
  
  # calculate the excesses w.r.t. the threshold
  excesses <- exceedances - threshold
  
  # calculate empirical pdf
  empirical_density_object <- EnvStats::epdfPlot(x = excesses, plot.it = FALSE)
  
  # extract ordered quantiles
  ordered_quantiles_excesses <- empirical_density_object$x
  
  # extract empirical pdf
  empirical_pdf <- empirical_density_object$f.x
  
  # calculate theoretical pdf
  theoretical_pdf <- calculate_gp_pdf(x = ordered_quantiles_excesses, 
                                      scale = gp_model_parameters["scale"], 
                                      shape = gp_model_parameters["shape"])
  
  # get the pdf range
  pdf_range <- range(c(theoretical_pdf, empirical_pdf))
  
  # plot densities
  plot(x = ordered_quantiles_excesses, 
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
  
  lines(ordered_quantiles_excesses, theoretical_pdf, col = 2, lwd = 2)
  
  abline(h = 0, lty = "dotted", lwd = 1)
  abline(v = 0, lty = "dotted", lwd = 1)
  
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
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)
# 
# threshold <- find_minimum_threshold(x)
# 
# gp_model <- estimate_gp_parameters(x, threshold)
# 
# plot_gp_pdf(gp_model)
# 
# 
# # example 2
# 
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# threshold <- find_minimum_threshold(x)
# 
# gp_model <- estimate_gp_parameters(x, threshold)
# 
# plot_gp_pdf(gp_model)
# 
# 
# # example 3
# 
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)
# 
# threshold <- find_minimum_threshold(x)
# 
# gp_model <- estimate_gp_parameters(x, threshold)
# 
# plot_gp_pdf(gp_model)
