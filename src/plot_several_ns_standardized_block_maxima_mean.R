source("./src/estimate_several_ns_standardized_block_maxima_mean.R")

plot_several_ns_standardized_block_maxima_mean <- function(x, 
                                                           block_sizes, 
                                                           confidence_level = 0.95,
                                                           equivalent = FALSE,
                                                           data = NULL, 
                                                           location.fun = ~1,
                                                           scale.fun = ~1, 
                                                           shape.fun = ~1, 
                                                           use.phi = TRUE,
                                                           type = c("GEV", "Gumbel")[1],
                                                           method = c("MLE", "GMLE")[1],
                                                           xlab = "Block Sizes", 
                                                           ylab = "Estimated Values", 
                                                           main = "Mean Standardized Block Maxima Plot"){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # confidence_interval_level: desired confidence level
  # equivalent: a boolean which indicates whether the equivalent estimates are returned or not
  # data: dataframe of covariates for linear modeling of the location parameter
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # type: type of model to use
  # method: estimation method to use
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  
  # estimate the mean of each required standardized block maxima
  estimated_mean_confidence_intervals_object <- estimate_several_ns_standardized_block_maxima_mean(x = x, 
                                                                                                   block_sizes = block_sizes, 
                                                                                                   confidence_level = confidence_level, 
                                                                                                   data = data, 
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
    abline(h = 0.5772156649, lty = 1, lwd = 1, col = 1)
  }
  
  legend(x = "topleft", 
         legend = c("CI Lower Bound", "Estimate", "CI Upper Bound", "Euler's constant"),
         lty = c(1, 1, 1, 1),
         col = c(2, 4, 3, 1),
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
# n <- 10000
# 
# x <- rnorm(n = n)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# plot_several_ns_standardized_block_maxima_mean(x, 
#                                                block_sizes, 
#                                                confidence_level = 0.95, 
#                                                equivalent = FALSE, 
#                                                data = data,
#                                                location.fun = ~ 1,
#                                                scale.fun = ~1,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[1])
# plot_several_ns_standardized_block_maxima_mean(x, 
#                                                block_sizes, 
#                                                confidence_level = 0.95, 
#                                                equivalent = TRUE, 
#                                                data = data,
#                                                location.fun = ~ 1,
#                                                scale.fun = ~1,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[1])
# 
# 
# # example 2
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# 
# n <- 10000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = +0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# plot_several_ns_standardized_block_maxima_mean(x, 
#                                                block_sizes, 
#                                                confidence_level = 0.95, 
#                                                equivalent = FALSE, 
#                                                data = data,
#                                                location.fun = ~ .,
#                                                scale.fun = ~1,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# plot_several_ns_standardized_block_maxima_mean(x, 
#                                                block_sizes, 
#                                                confidence_level = 0.95, 
#                                                equivalent = TRUE, 
#                                                data = data,
#                                                location.fun = ~ .,
#                                                scale.fun = ~1,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
