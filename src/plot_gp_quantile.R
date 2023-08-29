# library(EnvStats)

source("./src/estimate_gp_parameters.R")
source("./src/calculate_gp_inverse_cdf.R")

plot_gp_quantile <- function(gp_model, xlab = "Theoretical Quantile (Excesses)", ylab = "Empirical Quantile (Excesses)", main = "Quantile Plot"){
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
  
  # calculate empirical cdf
  empirical_probability_object <- EnvStats::ecdfPlot(x = excesses, prob.method ="plot.pos", plot.pos.con = 0.375, plot.it = FALSE)
  
  # extract empirical quantiles
  empirical_quantiles_excesses <- empirical_probability_object$Order.Statistics
  
  # extract empirical cdf
  empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
  
  # calculate theoretical quantiles
  theoretical_quantiles_excesses <- calculate_gp_inverse_cdf(p = empirical_cdf, 
                                                             scale = gp_model_parameters["scale"], 
                                                             shape = gp_model_parameters["shape"])
  
  # get the quantile range
  quantile_range <- range(c(theoretical_quantiles_excesses, empirical_quantiles_excesses)) 
  
  # plot quantile
  plot(x = theoretical_quantiles_excesses, 
       y = empirical_quantiles_excesses, 
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
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 100, loc = 1, scale = 0.5, shape = +0.2)
# 
# threshold <- find_minimum_threshold(x)
# 
# gp_model <- estimate_gp_parameters(x, threshold)
# 
# plot_gp_quantile(gp_model)
# 
# 
# # example 2
# 
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 100, loc = 1, scale = 0.5, shape = -0.2)
# 
# threshold <- find_minimum_threshold(x)
# 
# gp_model <- estimate_gp_parameters(x, threshold)
# 
# plot_gp_quantile(gp_model)
# 
# 
# # example 3
# 
# source("./src/generate_gp_sample.R")
# source("./src/find_minimum_threshold.R")
# 
# x <- generate_gp_sample(n = 100, loc = 1, scale = 0.5, shape = 0)
# 
# threshold <- find_minimum_threshold(x)
# 
# gp_model <- estimate_gp_parameters(x, threshold)
# 
# plot_gp_quantile(gp_model)
