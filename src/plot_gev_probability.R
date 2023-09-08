# library(EnvStats)

source("./src/estimate_single_gev_model.R")
source("./src/calculate_gev_cdf.R")

plot_gev_probability <- function(gev_model, xlab = "Theoretical Probability", ylab = "Empirical Probability", main = "Probability Plot"){
  # gev_model: an object associated with a result of the function "estimate_single_gev_model()"
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # extract the model parameters
  gev_model_parameters <- gev_model$normalized_gev_parameters
  
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
                                       loc = gev_model_parameters["loc_star"], 
                                       scale = gev_model_parameters["scale_star"], 
                                       shape = gev_model_parameters["shape_star"])
  
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



# example 1

source("./src/generate_gev_sample.R")

x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.1)

block_size <- 40

gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)

plot_gev_probability(gev_model)


# example 2

source("./src/generate_gev_sample.R")

x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.1)

block_size <- 40

gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)

plot_gev_probability(gev_model)


# example 3

source("./src/generate_gev_sample.R")

x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)

block_size <- 40

gev_model <- estimate_single_gev_model(x, block_size, nsloc = NULL)

plot_gev_probability(gev_model)
