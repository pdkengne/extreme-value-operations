# library(EnvStats)

source("./src/calculate_gev_cdf.R")

plot_gev_normalized_probability <- function(x, 
                                            loc = 0, 
                                            scale = 1, 
                                            shape = 0, 
                                            xlab = "Theoretical Probability", 
                                            ylab = "Empirical Probability", 
                                            main = "Probability Plot"){
  # x: vector of observations
  # loc, scale, shape: normalized location, scale and shape parameters of the considered gev distribution
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract train data
  uvdata <- x
  
  # calculate empirical cdf
  empirical_probability_object <- EnvStats::ecdfPlot(x = uvdata, prob.method ="plot.pos", plot.pos.con = 0.375, plot.it = FALSE)
  
  # extract ordered quantiles
  ordered_quantiles <- empirical_probability_object$Order.Statistics
  
  # extract empirical cdf
  empirical_cdf <- empirical_probability_object$Cumulative.Probabilities
  
  # calculate theoretical cdf
  theoretical_cdf <- calculate_gev_cdf(q = ordered_quantiles, 
                                       loc = loc, 
                                       scale = scale, 
                                       shape = shape)
  
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
# source("./src/estimate_single_gev_model.R")
# source("./src/generate_gev_sample.R")
# 
# loc <- 1
# 
# scale <- 0.5
# 
# shape <- 0.1
# 
# x <- generate_gev_sample(n = 1000, loc = loc, scale = scale, shape = shape)
# 
# plot_gev_normalized_probability(x, 
#                                 loc = loc, 
#                                 scale = scale, 
#                                 shape = shape, 
#                                 xlab = "Theoretical Probability", 
#                                 ylab = "Empirical Probability", 
#                                 main = "Probability Plot")
# 
# 
# # example 2
# 
# source("./src/estimate_single_gev_model.R")
# source("./src/generate_gev_sample.R")
# 
# loc <- 1
# 
# scale <- 0.5
# 
# shape <- -0.2
# 
# x <- generate_gev_sample(n = 1000, loc = loc, scale = scale, shape = shape)
# 
# plot_gev_normalized_probability(x, 
#                                 loc = loc, 
#                                 scale = scale, 
#                                 shape = shape, 
#                                 xlab = "Theoretical Probability", 
#                                 ylab = "Empirical Probability", 
#                                 main = "Probability Plot")
# 
# 
# # example 3
# 
# source("./src/estimate_single_gev_model.R")
# source("./src/generate_gev_sample.R")
# 
# loc <- 1
# 
# scale <- 0.5
# 
# shape <- 0
# 
# x <- generate_gev_sample(n = 1000, loc = loc, scale = scale, shape = shape)
# 
# plot_gev_normalized_probability(x, 
#                                 loc = loc, 
#                                 scale = scale, 
#                                 shape = shape, 
#                                 xlab = "Theoretical Probability", 
#                                 ylab = "Empirical Probability", 
#                                 main = "Probability Plot")
