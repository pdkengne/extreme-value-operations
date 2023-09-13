# library(EnvStats)

source("./src/estimate_single_gev_model.R")
source("./src/calculate_gev_pdf.R")
source("./src/find_threshold_associated_with_given_block_size.R")

plot_gev_pdf <- function(gev_model, 
                         zoom = FALSE,
                         xlab = "Quantile", 
                         ylab = "Density", 
                         main = "Probability Density Function (PDF) Plot"){
  # gev_model: an object associated with a result of the function "estimate_single_gev_model()"
  # zoom: a boolean which indicates whether to focus on large values or not
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the model parameters
  gev_model_parameters <- gev_model$normalized_gev_parameters
  
  # extract train data
  uvdata <- gev_model$data
  
  # extract block size
  block_size <- gev_model$block_size
  
  # find the threshold associated with the block_size
  threshold <- find_threshold_associated_with_given_block_size(x = uvdata, block_size = block_size)
  
  # calculate empirical pdf
  empirical_density_object <- EnvStats::epdfPlot(x = uvdata, plot.it = FALSE)
  
  # extract ordered quantiles
  ordered_quantiles <- empirical_density_object$x
  
  # extract empirical pdf
  empirical_pdf <- empirical_density_object$f.x
  
  # calculate theoretical pdf
  theoretical_pdf <- calculate_gev_pdf(x = ordered_quantiles, 
                                       loc = gev_model_parameters["loc_star"], 
                                       scale = gev_model_parameters["scale_star"], 
                                       shape = gev_model_parameters["shape_star"])
  
  # get the pdf range
  pdf_range <- range(c(theoretical_pdf, empirical_pdf))
  
  # get positions of large quantities
  position_large_quantities <- which(ordered_quantiles >= threshold)
  
  # extract large quantities
  large_ordered_quantiles <- ordered_quantiles[position_large_quantities]
  large_empirical_pdf <- empirical_pdf[position_large_quantities]
  large_theoretical_pdf <- theoretical_pdf[position_large_quantities]
  large_pdf_range <- range(c(large_theoretical_pdf, large_empirical_pdf))
  
  # plot densities
  if (zoom){
    plot(x = large_ordered_quantiles, 
         y = large_empirical_pdf, 
         type = "l", 
         ylim = large_pdf_range,
         col = 4,
         lwd = 2,
         cex.axis = 1,
         cex.lab = 1,
         cex.main = 1,
         xlab = xlab,
         ylab = ylab,
         main = paste(main,": zoom =", zoom))
  }
  else{
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
         main = paste(main,": zoom =", zoom))
  }
  
  lines(large_ordered_quantiles, large_theoretical_pdf, col = 2, lwd = 2)
  
  abline(h = 0, lty = "dotted", lwd = 1)
  abline(v = threshold, lty = "dotted", lwd = 1)
  
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
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.1)
# 
# block_size <- 40
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_pdf(gev_model,
#              zoom = FALSE,
#              xlab = "Quantile",
#              ylab = "Density",
#              main = "Probability Density Function (PDF) Plot")
# 
# plot_gev_pdf(gev_model,
#              zoom = TRUE,
#              xlab = "Quantile",
#              ylab = "Density",
#              main = "Probability Density Function (PDF) Plot")
# 
# gev_model$normalized_gev_parameters
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = -0.1)
# 
# block_size <- 40
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_pdf(gev_model,
#              zoom = FALSE,
#              xlab = "Quantile",
#              ylab = "Density",
#              main = "Probability Density Function (PDF) Plot")
# 
# plot_gev_pdf(gev_model,
#              zoom = TRUE,
#              xlab = "Quantile",
#              ylab = "Density",
#              main = "Probability Density Function (PDF) Plot")
# 
# gev_model$normalized_gev_parameters
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)
# 
# block_size <- 40
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_pdf(gev_model,
#              zoom = FALSE,
#              xlab = "Quantile",
#              ylab = "Density",
#              main = "Probability Density Function (PDF) Plot")
# 
# plot_gev_pdf(gev_model,
#              zoom = TRUE,
#              xlab = "Quantile",
#              ylab = "Density",
#              main = "Probability Density Function (PDF) Plot")
# 
# gev_model$normalized_gev_parameters
