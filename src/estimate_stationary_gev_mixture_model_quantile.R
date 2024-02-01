# library(extRemes)

source("./src/calculate_stationary_gev_mixture_model_inverse_cdf.R")
source("./src/estimate_gev_model_quantile.R")
source("./src/estimate_gev_parameters.R")

estimate_stationary_gev_mixture_model_quantile <- function(gev_mixture_model_object,
                                                           alpha,
                                                           do.ci = TRUE,
                                                           confidence_level = 0.95,
                                                           kind = c("geometric", "arithmetic", "harmonic")[1],
                                                           iterations = 100){
  # gev_mixture_model_object: an object associated with a result of the function "fit_stationary_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic" or "harmonic"
  # alpha: order of the quantile to estimate
  # do.ci: boolean which indicates whether to return confidence interval or not
  # confidence_level: the desired confidence level for the estimated quantile
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  # create an empty output object
  output <- list()
  
  # extract the dataset of all observations
  all_data <- gev_mixture_model_object$all_data
  
  # extract the sample of largest observations
  partial_data <- gev_mixture_model_object$partial_data
  
  # calculate the proportion of largest data
  partial_data_proportion <- length(partial_data)/length(all_data)
  
  # update the quantile order
  beta <- alpha/partial_data_proportion
  
  # calculate the quantile associated with the gev mixture model
  gev_mixture_model_quantile <- calculate_stationary_gev_mixture_model_inverse_cdf(gev_mixture_model_object,
                                                                                   p = 1 - beta, 
                                                                                   kind = kind,
                                                                                   iterations = iterations)
  
  # get the normalized gev parameters
  if (gev_mixture_model_object$use_extremal_index){
    normalized_gev_parameters <- gev_mixture_model_object$full_normalized_gev_parameters_object
  } 
  else{
    normalized_gev_parameters <- gev_mixture_model_object$normalized_gev_parameters_object
  }
  
  shapes <- normalized_gev_parameters$shape_star
  scales <- normalized_gev_parameters$scale_star
  locations <- normalized_gev_parameters$loc_star
  
  # extract the list of all estimated gev models
  gev_models_object <- gev_mixture_model_object$selected_gev_models
  
  # calculate the quantiles associated with all normalized gev models
  
  quantiles_object <- sapply(1:length(gev_models_object), function(k){
    # transform data to be at the standard gumbel distribution scale
    normalized_maxima <- extRemes::trans(gev_models_object[[k]])
    normalized_maxima <- normalized_maxima[is.finite(normalized_maxima)]
    
    # transform data to be at the specific gev distribution scale
    unnormalized_maxima <- extRemes::revtrans.evd(z = normalized_maxima,
                                                  location = locations[k], 
                                                  scale = scales[k], 
                                                  shape = shapes[k],
                                                  type = "GEV")
    
    # fit a gev model to the unnormalized maxima
    gev_model_star <- estimate_gev_parameters(x = unnormalized_maxima,
                                              type = c("GEV", "Gumbel")[1],
                                              method = c("MLE", "GMLE", "Lmoments")[1])
    
    # estimated the desired quantile with confidence interval
    quantile <- estimate_gev_model_quantile(gev_model = gev_model_star,
                                            alpha = beta,
                                            do.ci = do.ci,
                                            confidence_level = confidence_level)
    
    quantile
  })
  
  quantiles_object <- data.frame(t(quantiles_object))
  names(quantiles_object) <- c("lower_bound", "estimate", "upper_bound")
  rownames(quantiles_object) <- names(gev_models_object)
  
  # find the minimum of lower bounds associated with all confidence intervals
  smalest_lower_ci_bound <- min(quantiles_object)
  
  # find the maximum of lower bounds associated with all confidence intervals
  largest_upper_ci_bound <- max(quantiles_object)
  
  
  # transform data to be at the standard gumbel distribution scale
  several_standard_gumbel_residuals <- lapply(gev_models_object, function(model){
    standard_gumbel_residuals <- extRemes::trans(model)
    standard_gumbel_residuals
  })
  
  unified_standard_gumbel_residuals <- unlist(several_standard_gumbel_residuals)
  unified_standard_gumbel_residuals <- unified_standard_gumbel_residuals[is.finite(unified_standard_gumbel_residuals)]
  
  # transform data to be at the specific gev distribution scale
  unnormalized_maxima_gev_inf <- extRemes::revtrans.evd(z = unified_standard_gumbel_residuals,
                                                        location = max(locations), 
                                                        scale = max(scales), 
                                                        shape = max(shapes),
                                                        type = "GEV")
  
  unnormalized_maxima_gev_sup <- extRemes::revtrans.evd(z = unified_standard_gumbel_residuals,
                                                        location = min(locations), 
                                                        scale = min(scales), 
                                                        shape = min(shapes),
                                                        type = "GEV")
  
  # fit a gev model to the unnormalized maxima
  model_star_gev_inf <- estimate_gev_parameters(x = unnormalized_maxima_gev_inf,
                                                type = c("GEV", "Gumbel")[1],
                                                method = c("MLE", "GMLE", "Lmoments")[1])
  
  model_star_gev_sup <- estimate_gev_parameters(x = unnormalized_maxima_gev_sup,
                                                type = c("GEV", "Gumbel")[1],
                                                method = c("MLE", "GMLE", "Lmoments")[1])
  
  # estimated the desired quantile with confidence interval
  quantile_gev_inf <- estimate_gev_model_quantile(gev_model = model_star_gev_inf,
                                                  alpha = beta,
                                                  do.ci = do.ci,
                                                  confidence_level = confidence_level)
  
  quantile_gev_inf <- data.frame(quantile_gev_inf)
  names(quantile_gev_inf) <- c("lower_bound", "estimate", "upper_bound")
  
  quantile_gev_sup <- estimate_gev_model_quantile(gev_model = model_star_gev_sup,
                                                  alpha = beta,
                                                  do.ci = do.ci,
                                                  confidence_level = confidence_level)
  
  quantile_gev_sup <- data.frame(quantile_gev_sup)
  names(quantile_gev_sup) <- c("lower_bound", "estimate", "upper_bound")
  
  # update the output object
  output[["quantiles_object"]] <- quantiles_object
  output[["gev_mixture_model_quantile"]] <- gev_mixture_model_quantile
  output[["smalest_lower_ci_bound"]] <- smalest_lower_ci_bound
  output[["largest_upper_ci_bound"]] <- largest_upper_ci_bound
  output[["quantile_gev_inf"]] <- quantile_gev_inf
  output[["quantile_gev_sup"]] <- quantile_gev_sup
  
  output
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
# results_geometric <- estimate_stationary_gev_mixture_model_quantile(gev_mixture_model_object,
#                                                                     alpha = alpha,
#                                                                     do.ci = TRUE,
#                                                                     confidence_level = 0.95,
#                                                                     kind = c("geometric", "arithmetic", "harmonic")[1],
#                                                                     iterations = 100)
# 
# results_geometric
# 
# results_arithmetic <- estimate_stationary_gev_mixture_model_quantile(gev_mixture_model_object,
#                                                                      alpha = alpha,
#                                                                      do.ci = TRUE,
#                                                                      confidence_level = 0.95,
#                                                                      kind = c("geometric", "arithmetic", "harmonic")[2],
#                                                                      iterations = 100)
# 
# results_arithmetic
# 
# results_harmonic <- estimate_stationary_gev_mixture_model_quantile(gev_mixture_model_object,
#                                                                      alpha = alpha,
#                                                                      do.ci = TRUE,
#                                                                      confidence_level = 0.95,
#                                                                      kind = c("geometric", "arithmetic", "harmonic")[3],
#                                                                      iterations = 100)
# 
# results_harmonic
