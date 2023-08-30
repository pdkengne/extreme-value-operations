# library(EnvStats)

source("./src/estimate_gev_parameters.R")
source("./src/calculate_gev_cdf.R")

plot_gev_probability <- function(gev_model, xlab = "Theoretical Probability", ylab = "Empirical Probability", main = "Probability Plot"){
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
  
  # get the probability range
  probability_range <- range(c(theoretical_cdf, empirical_cdf)) 
  
  # plot probability
  plot(x = theoretical_cdf, 
       y = empirical_cdf, 
       type = "p", 
       ylim = probability_range,
       xlim = probability_range,
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
# x <- generate_gev_sample(n = 100, loc = 1, scale = 0.5, shape = +0.2)
# 
# gev_model <- estimate_gev_parameters(x, nsloc = NULL)
# 
# plot_gev_probability(gev_model)
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 100, loc = 1, scale = 0.5, shape = -0.2)
# 
# gev_model <- estimate_gev_parameters(x, nsloc = NULL)
# 
# plot_gev_probability(gev_model)
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 100, loc = 1, scale = 0.5, shape = 0)
# 
# gev_model <- estimate_gev_parameters(x, nsloc = NULL)
# 
# plot_gev_probability(gev_model)
