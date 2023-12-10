# library(extRemes)

source("./src/calculate_modes.R")
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/get_full_ns_gev_mixture_model_cdf.R")


plot_fit_ns_gev_mixture_model <- function(ns_gev_mixture_model_object, log_scale = TRUE){
  # ns_gev_mixture_model_object: an object associated with a result of the function "fit_ns_gev_mixture_model()"
  
  empirical_quantiles <- sort(get_full_ns_gev_mixture_model_cdf(ns_gev_mixture_model_object, 
                                                                kind = c("geometric", "arithmetic")[2]))
  
  n <- length(empirical_quantiles)
  
  theoretical_quantiles <- 1:n/(n+1)
  
  if (log_scale){
    plot(x = -log(theoretical_quantiles)/log(10), 
         y = -log(empirical_quantiles)/log(10),
         type = "p",
         lwd = 2,
         col = 4,
         xlab = "theoretical quantiles: -log10(expected)",
         ylab = "empirical quantiles: -log10(observed)",
         main = "quantile plot of residuals against the standard uniform distribution")
  } else{
    plot(x = theoretical_quantiles, 
         y = empirical_quantiles,
         type = "p",
         lwd = 2,
         col = 4,
         xlab = "theoretical quantiles",
         ylab = "empirical quantiles",
         main = "quantile plot of residuals against the standard uniform distribution")
  }
  
  abline(a = 0, b = 1, col = 2, lwd = 2)
  legend("topleft", legend = c("1-1 line"), lty = 1, col = 2)  

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
# ns_gev_mixture_model_object <- fit_ns_gev_mixture_model(x = x,
#                                                         data = data,
#                                                         location.fun = ~ scaled_waiting,
#                                                         scale.fun = ~ 1,
#                                                         shape.fun = ~ 1,
#                                                         use.phi = FALSE,
#                                                         nb_gev_models = p,
#                                                         min_cluster_size = 20,
#                                                         max_iteration = 50,
#                                                         tolerance = 10^(-3),
#                                                         left_cluster_extension_size = 5,
#                                                         right_cluster_extension_size = 10)
# 
# ns_gev_mixture_model_object$cluster_gev_model_coefficients
# 
# plot_fit_ns_gev_mixture_model(ns_gev_mixture_model_object, log_scale = TRUE)
# 
# plot_fit_ns_gev_mixture_model(ns_gev_mixture_model_object, log_scale = FALSE)

