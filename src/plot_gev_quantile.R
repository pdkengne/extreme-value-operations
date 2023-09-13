# library(EnvStats)

source("./src/estimate_single_gev_model.R")
source("./src/calculate_gev_inverse_cdf.R")

plot_gev_quantile <- function(gev_model, 
                              xlab = "Theoretical Quantile", 
                              ylab = "Empirical Quantile", 
                              main = "Quantile Plot"){
  # gev_model: an object associated with a result of the function "estimate_single_gev_model()"
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the model parameters
  gev_model_parameters <- gev_model$gev_model$estimate
  
  # extract train block maxima data
  uvdata <- gev_model$gev_model$data
  
  # calculate empirical cdf
  empirical_probability_object <- EnvStats::ecdfPlot(x = uvdata, prob.method ="plot.pos", plot.pos.con = 0.375, plot.it = FALSE)
  
  # extract empirical quantiles
  empirical_quantiles <- empirical_probability_object$Order.Statistics
  
  # extract empirical cdf
  empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
  
  # calculate theoretical quantiles
  theoretical_quantiles <- calculate_gev_inverse_cdf(p = empirical_cdf, 
                                                     loc = gev_model_parameters["loc"], 
                                                     scale = gev_model_parameters["scale"], 
                                                     shape = gev_model_parameters["shape"])
  
  # get the quantile range
  quantile_range <- range(c(theoretical_quantiles, empirical_quantiles)) 
  
  # plot quantile
  plot(x = theoretical_quantiles, 
       y = empirical_quantiles, 
       type = "p", 
       ylim = quantile_range,
       xlim = quantile_range,
       pch = 20,
       col = 4,
       lwd = 2,
       cex.axis = 1,
       cex.lab = 1,
       cex.main = 1,
       xlab = xlab,
       ylab = ylab,
       main = main
  )
  
  abline(a = 0, b = 1, lty = "dotted", lwd = 2)
  
  legend(x = "topleft", 
         legend = c("Line y = x"),
         lty = 3,
         col = 1,
         lwd = 2,
         title = "Legend"
  )
  
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = +0.2)
# 
# block_size <- 100
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_quantile(gev_model,
#                   xlab = "Theoretical Quantile",
#                   ylab = "Empirical Quantile",
#                   main = "Quantile Plot")
# 
# gev_model$normalized_gev_parameters
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = -0.2)
# 
# block_size <- 100
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_quantile(gev_model,
#                   xlab = "Theoretical Quantile",
#                   ylab = "Empirical Quantile",
#                   main = "Quantile Plot")
# 
# gev_model$normalized_gev_parameters
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = 0)
# 
# block_size <- 100
# 
# gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)
# 
# plot_gev_quantile(gev_model,
#                   xlab = "Theoretical Quantile",
#                   ylab = "Empirical Quantile",
#                   main = "Quantile Plot")
# 
# gev_model$normalized_gev_parameters
