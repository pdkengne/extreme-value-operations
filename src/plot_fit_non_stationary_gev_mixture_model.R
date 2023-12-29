source("./src/get_ns_gev_model_parameters.R")
source("./src/calculate_gev_mixture_model_cdf.R")
source("./src/calculate_non_stationary_gev_mixture_model_cdf.R")


plot_fit_non_stationary_gev_mixture_model <- function(ns_gev_mixture_model_object,
                                                      type = c("quantile", "histogram")[1],
                                                      kind = c("geometric", "arithmetic")[2]){
  # ns_gev_mixture_model_object: an object associated with a result of the function "fit_non_stationary_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic"
  # type: indicates the type of diagnostic to plot. Possible values are : "quantile" or "histogram"
  
  theoretical_quantiles <- sort(calculate_non_stationary_gev_mixture_model_cdf(ns_gev_mixture_model_object,
                                                                               q = NULL,
                                                                               data = NULL,
                                                                               kind = kind))
  threshold <- ns_gev_mixture_model_object$threshold
  
  n <- length(theoretical_quantiles)
  
  empirical_quantiles <- 1:n/(n+1)
  
  if (type == "quantile"){
    plot(x = empirical_quantiles, 
         y = theoretical_quantiles,
         type = "p",
         lwd = 2,
         col = 4,
         ylab = "theoretical quantiles",
         xlab = "empirical quantiles",
         main = "quantile plot of residuals against the standard uniform distribution")
    
    abline(a = 0, b = 1, col = 2, lwd = 2)
    abline(v = threshold, lty = "dotted", lwd = 2)
    
    legend("topleft", legend = c("1-1 line"), lty = 1, col = 2)  
  }
  else if (type == "histogram"){
    hist(theoretical_quantiles, 
         freq = FALSE, 
         lwd = 2,
         col = 4,
         ylab = "density",
         xlab = "support",
         main = "histogram of residuals")
    
    abline(v = threshold, lty = "dotted", lwd = 2)
  }
}
  


# example 1

source("./src/fit_non_stationary_gev_mixture_model.R")
source("./src/generate_gev_sample.R")

n <- 10000

x <- rnorm(n = n)

#x <- rexp(n = n, rate = 1)

#x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.01)

#x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.01)

ns_gev_mixture_model_object <- fit_non_stationary_gev_mixture_model(x = x,
                                                                    data = NULL,
                                                                    nlargest = 3000,
                                                                    block_sizes = NULL,
                                                                    minimum_nblocks = 50,
                                                                    threshold = NULL,
                                                                    confidence_level = 0.95,
                                                                    use_extremal_index = TRUE,
                                                                    use_uniform_prior = TRUE,
                                                                    method = c("MLE", "GMLE")[1])

plot_fit_non_stationary_gev_mixture_model(ns_gev_mixture_model_object,
                                          type = c("quantile", "histogram")[1],
                                          kind = c("geometric", "arithmetic")[2])

plot_fit_non_stationary_gev_mixture_model(ns_gev_mixture_model_object,
                                          type = c("quantile", "histogram")[2],
                                          kind = c("geometric", "arithmetic")[1])



