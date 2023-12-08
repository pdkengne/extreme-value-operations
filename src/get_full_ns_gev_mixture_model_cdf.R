# library(extRemes)

source("./src/get_ns_gev_model_parameters.R")
source("./src/calculate_gev_mixture_model_cdf.R")

get_full_ns_gev_mixture_model_cdf <- function(ns_gev_mixture_model, kind = c("geometric", "arithmetic")[1]){
  # ns_gev_mixture_model: an object associated with a result of the function "fit_ns_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  
  p <- ns_gev_mixture_model$nclusters
  
  x <- ns_gev_mixture_model$data
  
  n <- length(x)
  
  covariates <- ns_gev_mixture_model$covariates
  
  cluster_weights <- ns_gev_mixture_model$cluster_weights
  
  cluster_models <- ns_gev_mixture_model$cluster_models
  
  cluster_parameters <- lapply(cluster_models, function(model){
    parameters <- get_ns_gev_model_parameters(model, covariates)
    parameters
  })
  
  mixture_distributions <- sapply(1:n, function(i){
    obs <- x[i]
    
    distributions <- sapply(1:p, function(k){
      parameters_list <- cluster_parameters[[k]]
      
      locations <- parameters_list$location
      scales <- parameters_list$scale
      shapes <- parameters_list$shape

      coefficients <- c(locations[i], scales[i], shapes[i])
      names(coefficients) <- c("location", "scale", "shape")
      
      coefficients
    })
    
    cdf <- calculate_gev_mixture_model_cdf(q = obs,
                                           locations = distributions["location", ],
                                           scales = distributions["scale", ],
                                           shapes = distributions["shape", ],
                                           weights = cluster_weights,
                                           kind = kind)

    cdf
  })
  
  mixture_distributions
}



# # example 1
# 
# source("./src/fit_ns_gev_mixture_model.R")
# source("./src/calculate_modes.R")
# source("./src/plot_modes.R")
# 
# data(faithful, package = "datasets")
# 
# data <- faithful
# 
# data$scaled_waiting <- scale(data$waiting)
# 
# names(data)
# 
# x <- data$eruptions
# 
# modes_object <- calculate_modes(x = x)
# 
# plot_modes(modes_object)
# 
# p <- 2
# 
# ns_gev_mixture_model <- fit_ns_gev_mixture_model(x = x,
#                                                  data = data,
#                                                  location.fun = ~ scaled_waiting,
#                                                  scale.fun = ~ 1,
#                                                  shape.fun = ~ 1,
#                                                  use.phi = FALSE,
#                                                  nb_gev_models = p,
#                                                  min_cluster_size = 20,
#                                                  max_iteration = 50,
#                                                  tolerance = 10^(-3),
#                                                  left_cluster_extension_size = 5,
#                                                  right_cluster_extension_size = 10)
# 
# ns_gev_mixture_model$cluster_gev_model_coefficients
# 
# results_1 <- get_full_ns_gev_mixture_model_cdf(ns_gev_mixture_model, kind = c("geometric", "arithmetic")[1])
# 
# results_1
# 
# hist(results_1)
# 
# ks.test(x = results_1 ,y = "punif", min = 0, max = 1)
# 
# 
# results_2 <- get_full_ns_gev_mixture_model_cdf(ns_gev_mixture_model, kind = c("geometric", "arithmetic")[2])
# 
# results_2
# 
# hist(results_2)
# 
# ks.test(x = results_2 ,y = "punif", min = 0, max = 1)
# 
# 
# theoretical_quantiles <- 1:n/(n+1)
# empirical_quantiles <- sort(results_2)
# 
# plot(x = theoretical_quantiles, 
#      y = empirical_quantiles,
#      type = "p",
#      lwd = 3,
#      xlab = "theoretical quantiles",
#      ylab = "empirical quantiles",
#      main = "quantile plot of residuals against the standard uniform distribution")
# 
# abline(a = 0, b = 1, col = 2, lwd = 2)
# 
# 
# plot(x = -log(theoretical_quantiles)/log(10), 
#      y = -log(empirical_quantiles)/log(10),
#      type = "p",
#      lwd = 3,
#      col = 4,
#      xlab = "theoretical quantiles: -log10(observed)",
#      ylab = "empirical quantiles: -log10(expected)",
#      main = "quantile plot of residuals against the standard uniform distribution")
# 
# abline(a = 0, b = 1, col = 2, lwd = 2)
# 
# 
# 
# library(gap)
# 
# ?qqunif
# 
# gap::qqunif(u = results_2,
#             type = "unif",
#             logscale = TRUE,
#             base = 10,
#             col = palette()[4],
#             lcol = palette()[2],
#             ci = TRUE,
#             alpha = 0.05)


