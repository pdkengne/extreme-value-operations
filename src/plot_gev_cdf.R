# library(EnvStats)

source("./src/estimate_gev_parameters.R")
source("./src/calculate_gev_cdf.R")

plot_gev_cdf <- function(gev_model, xlab = "Quantile", ylab = "Cumulative Probability", main = "Cumulative Distribution Function (CDF) Plot"){
  # gev_model: an object associated with a result of the function "estimate_gev_parameters()"
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the model parameters
  gev_model_parameters <- gev_model$estimate
  
  # extract train data
  uvdata <- gev_model$data
  
  # calculate empirical cdf
  empirical_probability_object <- EnvStats::ecdfPlot(x = uvdata, prob.method ="plot.pos", plot.pos.con = 0.375, plot.it = FALSE)
  
  # extract ordered quantiles
  ordered_quantiles <- empirical_probability_object$Order.Statistics
  
  # extract empirical cdf
  empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
  
  # calculate theoretical cdf
  theoretical_cdf <- calculate_gev_cdf(q = ordered_quantiles, 
                                       loc = gev_model_parameters["loc"], 
                                       scale = gev_model_parameters["scale"], 
                                       shape = gev_model_parameters["shape"])
  
  # get the cdf range
  cdf_range <- range(c(theoretical_cdf, empirical_cdf))
  
  # plot densities
  plot(x = ordered_quantiles, 
       y = empirical_cdf, 
       type = "l", 
       ylim = cdf_range,
       col = 4,
       lwd = 2,
       cex.axis = 1,
       cex.lab = 1,
       cex.main = 1,
       xlab = xlab,
       ylab = ylab,
       main = main
  )
  
  lines(ordered_quantiles, theoretical_cdf, col = 2, lwd = 2)
  
  abline(h = 0, lty = "dotted", lwd = 1)
  abline(h = 1, lty = "dotted", lwd = 1)
  
  legend(x = "right", 
         legend = c("Empirical CDF", "Theoretical CDF"),
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
# plot_gev_cdf(gev_model)
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
# plot_gev_cdf(gev_model)
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
# plot_gev_cdf(gev_model)
