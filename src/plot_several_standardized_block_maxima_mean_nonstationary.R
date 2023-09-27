source("./src/estimate_several_standardized_block_maxima_mean.R")
source("./src/estimate_several_standardized_block_maxima_mean_nonstationary.R")

plot_several_standardized_block_maxima_mean_nonstationary <- function(x, 
                                                        block_sizes, 
                                                        confidence_level = 0.95,
                                                        equivalent = FALSE,
                                                        data = NULL, 
                                                        threshold = NULL, 
                                                        threshold.fun = ~1, 
                                                        location.fun = ~1,
                                                        scale.fun = ~1, 
                                                        shape.fun = ~1, 
                                                        use.phi = FALSE,
                                                        type = c("GEV", "GP", "PP", "Gumbel", "Exponential")[1],
                                                        method = c("MLE", "GMLE", "Bayesian", "Lmoments")[1],
                                                        xlab = "Block Sizes", 
                                                        ylab = "Estimated Values", 
                                                        main = "Mean Standardized Block Maxima Plot"){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # confidence_interval_level: desired confidence level
  # equivalent: a boolean which indicates whether the equivalent estimates are returned or not
  # data: dataframe of covariates for linear modeling of the location parameter
  # threshold.fun, location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                                    must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter or not
  # type: type of model to use
  # method: estimation method to use
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # estimate the mean of each required standardized block maxima
  estimated_mean_confidence_intervals_object <- estimate_several_standardized_block_maxima_mean_nonstationary(x = x, 
                                                                                                              block_sizes = block_sizes, 
                                                                                                              confidence_level = confidence_level,
                                                                                                              data = data,
                                                                                                              threshold = threshold, 
                                                                                                              threshold.fun = threshold.fun, 
                                                                                                              location.fun = location.fun,
                                                                                                              scale.fun = scale.fun, 
                                                                                                              shape.fun = shape.fun, 
                                                                                                              use.phi = use.phi,
                                                                                                              type = type,
                                                                                                              method = method)
  
  if (equivalent == TRUE){
    estimated_mean_confidence_intervals <- estimated_mean_confidence_intervals_object$selected
    block_sizes <- as.numeric(rownames(estimated_mean_confidence_intervals))
    main <- paste("Equivalent", main)
  }
  else{
    estimated_mean_confidence_intervals <- estimated_mean_confidence_intervals_object$estimates
  }
  
  # extract a common interval to all intervals from the largest subset of overlapping intervals
  common_interval <- estimated_mean_confidence_intervals_object$common_interval
  
  # plot the estimated mean of each required standardized block maxima
  plot(x = block_sizes, 
       y = estimated_mean_confidence_intervals$estimate, 
       type = "l", 
       ylim = range(estimated_mean_confidence_intervals),
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
  
  lines(block_sizes, estimated_mean_confidence_intervals$lower_bound, col = 2, lwd = 2)
  lines(block_sizes, estimated_mean_confidence_intervals$upper_bound, col = 3, lwd = 2)
  
  if (equivalent == TRUE){
    abline(h = common_interval, lty = "dotted", lwd = 1, col = 7)
  }
  
  legend(x = "topleft", 
         legend = c("CI Lower Bound", "Estimate", "CI Upper Bound"),
         lty = c(1, 1, 1),
         col = c(2, 4, 3),
         lwd = 2,
         box.lty = 2,
         box.lwd = 1, 
         box.col = 1,
         title = "Legend"
  )

}



# # example 1
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# 
# x <- rnorm(n = 10000)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# plot_several_standardized_block_maxima_mean_nonstationary(x, block_sizes, confidence_level = 0.95, equivalent = FALSE)
# plot_several_standardized_block_maxima_mean_nonstationary(x, block_sizes, confidence_level = 0.95, equivalent = TRUE)
# 
# 
# # example 2
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = +0.2)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# plot_several_standardized_block_maxima_mean_nonstationary(x, block_sizes, confidence_level = 0.95, equivalent = FALSE)
# plot_several_standardized_block_maxima_mean_nonstationary(x, block_sizes, confidence_level = 0.95, equivalent = TRUE)
# 
# 
# # example 3
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 10000, loc = 1, scale = 0.5, shape = -0.2)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# plot_several_standardized_block_maxima_mean_nonstationary(x, block_sizes, confidence_level = 0.95, equivalent = FALSE)
# plot_several_standardized_block_maxima_mean_nonstationary(x, block_sizes, confidence_level = 0.95, equivalent = TRUE)
# 
# 
# # example 4
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# 
# x <- EnvStats::rzmnorm(n = 10000, mean = 0, sd = 1, p.zero = 0.5)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# plot_several_standardized_block_maxima_mean_nonstationary(x, block_sizes, confidence_level = 0.95, equivalent = FALSE)
# plot_several_standardized_block_maxima_mean_nonstationary(x, block_sizes, confidence_level = 0.95, equivalent = TRUE)
