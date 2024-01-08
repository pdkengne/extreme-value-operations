source("./src/estimate_stationary_gev_mixture_model_quantile.R")

plot_estimate_stationary_gev_mixture_model_quantile <- function(gev_mixture_model_object,
                                                                alpha,
                                                                true_quantile = NULL,
                                                                do.ci = TRUE,
                                                                confidence_level = 0.95,
                                                                kind = c("geometric", "arithmetic", "harmonic")[1],
                                                                iterations = 100,
                                                                xlab = "block size",
                                                                ylab = "estimates",
                                                                main = "quantile estimation plot",
                                                                legend_position = "topright"){
  # gev_mixture_model_object: an object associated with a result of the function "fit_stationary_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic" or "harmonic"
  # alpha: order of the quantile to estimate
  # do.ci: boolean which indicates whether to return confidence interval or not
  # confidence_level: the desired confidence level for the estimated quantile
  # iterations: number of iterations to perform in the the dichotomy algorithm
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  # legend_position: position of the legend
  
  quantile_estimation <- estimate_stationary_gev_mixture_model_quantile(gev_mixture_model_object = gev_mixture_model_object,
                                                                        alpha = alpha,
                                                                        do.ci = do.ci,
                                                                        confidence_level = confidence_level,
                                                                        kind = kind,
                                                                        iterations = iterations)
  
  selected_block_sizes <- gev_mixture_model_object$selected_block_sizes
  
  par(mar = c(5, 5, 7, 1), xpd = TRUE)
  
  matplot(x = selected_block_sizes,
          y = quantile_estimation$quantiles_object,
          type = "l",
          main = main,
          xlab = xlab,
          ylab = ylab,
          lty  = c("solid", "solid", "solid"),
          lwd = 2,
          col = c(7, 3, 7))
  
  matpoints(x = selected_block_sizes,
            y = quantile_estimation$quantiles_object,
            pch = 19,
            col = c(7, 3, 7), 
            lwd = 2)
  
  lines(x = selected_block_sizes,
        y = rep(x = quantile_estimation$smalest_lower_ci_bound, times = length(selected_block_sizes)),
        col = 1, 
        lwd = 1,
        lty = "dotted")
  
  lines(x = selected_block_sizes,
        y = rep(x = quantile_estimation$largest_upper_ci_bound, times = length(selected_block_sizes)),
        col = 1, 
        lwd = 1,
        lty = "dotted")
  
  lines(x = selected_block_sizes,
        y = rep(x = quantile_estimation$gev_mixture_model_quantile, times = length(selected_block_sizes)),
        col = 6, 
        lwd = 2)
  
  points(x = selected_block_sizes,
         y = rep(x = quantile_estimation$gev_mixture_model_quantile, times = length(selected_block_sizes)),
         col = 6, 
         pch = 19,
         lwd = 2)
  
  lines(x = selected_block_sizes,
        y = rep(x = true_quantile, times = length(selected_block_sizes)),
        col = 4, 
        lwd = 2)
  
  points(x = selected_block_sizes,
         y = rep(x = true_quantile, times = length(selected_block_sizes)),
         col = 4, 
         pch = 19,
         lwd = 2)
  
  legend(legend_position, 
         inset = c(0, -0.35),
         legend = c("true_quantile", "mixture_model_quantile", "single_model_quantile", "confidence_interval"),
         lty = c(1, 1, 1, 1), 
         col = c(4, 6, 3, 7), 
         lwd = 2, 
         pch = 19)

  }


# # example 1
# 
# source("./src/extract_nlargest_sample.R")
# source("./src/fit_stationary_gev_mixture_model.R")
# source("./src/generate_gev_sample.R")
# source("./src/plot_fit_stationary_gev_mixture_model.R")
# 
# n <- 10000
# 
# x <- rnorm(n = n)
# 
# #x <- rexp(n = n, rate = 1)
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.01)
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.01)
# 
# nlargest <- 3000
# nlargest_data <- extract_nlargest_sample(x = x, n = nlargest)
# DescTools::Desc(nlargest_data)
# 
# gev_mixture_model_object <- fit_stationary_gev_mixture_model(x = x,
#                                                              nlargest = nlargest,
#                                                              block_sizes = NULL,
#                                                              minimum_nblocks = 50,
#                                                              threshold = NULL,
#                                                              confidence_level = 0.95,
#                                                              use_extremal_index = TRUE,
#                                                              use_uniform_prior = TRUE,
#                                                              method = c("MLE", "GMLE", "Lmoments")[1])
# 
# gev_mixture_model_object$unnormalized_gev_parameters_object
# gev_mixture_model_object$weights
# gev_mixture_model_object$threshold
# 
# plot_fit_stationary_gev_mixture_model(gev_mixture_model_object,
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
# 
# alpha <- n^(-2)
# 
# true_quantile <- qnorm(p = 1 - alpha)
# true_quantile
# 
# plot_estimate_stationary_gev_mixture_model_quantile(gev_mixture_model_object,
#                                                     alpha = alpha,
#                                                     true_quantile = NULL,
#                                                     do.ci = TRUE,
#                                                     confidence_level = 0.95,
#                                                     kind = c("geometric", "arithmetic", "harmonic")[1],
#                                                     iterations = 100,
#                                                     xlab = "block sizes",
#                                                     ylab = "estimates",
#                                                     main = "quantile estimation plot",
#                                                     legend_position = "topright")
# 
# 
# plot_estimate_stationary_gev_mixture_model_quantile(gev_mixture_model_object,
#                                                     alpha = alpha,
#                                                     true_quantile = true_quantile,
#                                                     do.ci = TRUE,
#                                                     confidence_level = 0.95,
#                                                     kind = c("geometric", "arithmetic", "harmonic")[1],
#                                                     iterations = 100,
#                                                     xlab = "block sizes",
#                                                     ylab = "estimates",
#                                                     main = "quantile estimation plot",
#                                                     legend_position = "topright")
