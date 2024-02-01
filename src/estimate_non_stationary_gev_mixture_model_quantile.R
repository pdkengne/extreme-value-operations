# library(extRemes)

source("./src/calculate_non_stationary_gev_mixture_model_inverse_cdf.R")
source("./src/get_several_ns_gev_model_normalized_parameters.R")
source("./src/estimate_gev_model_quantile.R")
source("./src/estimate_gev_parameters.R")

estimate_non_stationary_gev_mixture_model_quantile <- function(ns_gev_mixture_model_object,
                                                               alpha,
                                                               data = NULL,
                                                               do.ci = TRUE,
                                                               confidence_level = 0.95,
                                                               kind = c("geometric", "arithmetic", "harmonic")[1],
                                                               iterations = 100){
  # ns_gev_mixture_model_object: an object associated with a result of the function "fit_non_stationary_gev_mixture_model()"
  # kind: indicates the type of gev mixture model. Possible values are "geometric" or "arithmetic" or "harmonic"
  # alpha: order of the quantile to estimate
  # data: dataframe of covariates for linear modeling of the gev model parameters
  # do.ci: boolean which indicates whether to return confidence interval or not
  # confidence_level: the desired confidence level for the estimated quantile
  # iterations: number of iterations to perform in the the dichotomy algorithm
  
  # extract the selected full non-stationary gev models 
  selected_full_ns_gev_models <- ns_gev_mixture_model_object$selected_full_ns_gev_models
  
  # extract the list of all estimated gev models
  selected_ns_gev_models <- ns_gev_mixture_model_object$selected_ns_gev_models
  
  # extract the dataset of all observations
  all_data <- ns_gev_mixture_model_object$all_data
  
  # extract the sample of largest observations
  partial_data <- ns_gev_mixture_model_object$partial_data
  
  # extract the dataset of covariates associated with the largest dataset
  partial_data_covariates <- ns_gev_mixture_model_object$partial_data_covariates
  
  # extract the threshold
  # threshold <- ns_gev_mixture_model_object$threshold
  
  # extract the vector of observations to use
  # observations <- partial_data[partial_data > threshold]
  
  # calculate the proportion of largest data
  # partial_data_proportion <- length(observations)/length(all_data)
  partial_data_proportion <- length(partial_data)/length(all_data)
  
  # update the quantile order
  beta <- alpha/partial_data_proportion
  
  # extract the dataset of covariates to use
  if (is.null(data)){
    index <- which.max(partial_data)
    data <- dplyr::slice(partial_data_covariates, index)
  }
  
  # calculate the normalized gev parameters
  if (ns_gev_mixture_model_object$use_extremal_index){
    normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                data = data,
                                                                                use_extremal_index = TRUE,
                                                                                normalize_parameters = TRUE)
  } 
  else{
    normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                data = data,
                                                                                use_extremal_index = FALSE,
                                                                                normalize_parameters = TRUE)
  }
  
  
  # calculate the quantile associated with the gev mixture model
  mixture_distributions <- lapply(1:nrow(data), function(i){
    # create an empty output object
    output <- list()
    
    # select one row of the covariates dataset to use
    selected_row_data <- dplyr::slice(data, i)
    
    # calculate the quantile associated with the gev mixture model
    gev_mixture_model_quantile <- calculate_non_stationary_gev_mixture_model_inverse_cdf(ns_gev_mixture_model_object,
                                                                                         p = 1 - beta,
                                                                                         data = selected_row_data,
                                                                                         kind = kind,
                                                                                         iterations = iterations)
    
    # calculate the quantiles associated with all normalized gev models
    quantiles_object <- sapply(1:length(selected_ns_gev_models), function(k){
      # transform data to be at the standard gumbel distribution scale
      normalized_maxima <- extRemes::trans(selected_ns_gev_models[[k]])
      normalized_maxima <- normalized_maxima[is.finite(normalized_maxima)]
      
      # transform data to be at the specific gev distribution scale
      parameters <- normalized_gev_parameters[[k]]
      unnormalized_maxima <- extRemes::revtrans.evd(z = normalized_maxima,
                                                    location = parameters$location[i], 
                                                    scale = parameters$scale[i], 
                                                    shape = parameters$shape[i],
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
    rownames(quantiles_object) <- names(selected_ns_gev_models)
    
    # find the minimum of lower bounds associated with all confidence intervals
    smalest_lower_ci_bound <- min(quantiles_object)
    
    # find the maximum of lower bounds associated with all confidence intervals
    largest_upper_ci_bound <- max(quantiles_object)
    
    
    # transform data to be at the standard gumbel distribution scale
    several_standard_gumbel_residuals <- lapply(selected_ns_gev_models, function(model){
      standard_gumbel_residuals <- extRemes::trans(model)
      standard_gumbel_residuals
    })
    
    unified_standard_gumbel_residuals <- unlist(several_standard_gumbel_residuals)
    unified_standard_gumbel_residuals <- unified_standard_gumbel_residuals[is.finite(unified_standard_gumbel_residuals)]
    
    # extract the current normalized gev model parameters
    locations <- sapply(normalized_gev_parameters, function(params){
      params[i, "location"]
    })
    
    scales <- sapply(normalized_gev_parameters, function(params){
      params[i, "scale"]
    })
    
    shapes <- sapply(normalized_gev_parameters, function(params){
      params[i, "shape"]
    })
    
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
  })
  
  names(mixture_distributions) <- paste0("covariates_", rownames(data), sep = "")
  
  mixture_distributions
}



# # example 1
# 
# source("./src/extract_nlargest_sample.R")
# source("./src/fit_non_stationary_gev_mixture_model.R")
# source("./src/generate_gev_sample.R")
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
# ns_gev_mixture_model_object <- fit_non_stationary_gev_mixture_model(x = x,
#                                                                     data = NULL,
#                                                                     nlargest = 3000,
#                                                                     block_sizes = NULL,
#                                                                     minimum_nblocks = 50,
#                                                                     threshold = NULL,
#                                                                     confidence_level = 0.95,
#                                                                     use_extremal_index = TRUE,
#                                                                     use_uniform_prior = TRUE,
#                                                                     method = c("MLE", "GMLE")[1])
# 
# 
# ns_gev_mixture_model_object$selected_ns_gev_coefficients
# ns_gev_mixture_model_object$weights
# 
# alpha <- n^(-2)
# 
# true_quantile <- qnorm(p = 1 - alpha)
# true_quantile
# 
# data <- dplyr::slice(ns_gev_mixture_model_object$all_data_covariates, 1:2)
# 
# results_geometric <- estimate_non_stationary_gev_mixture_model_quantile(ns_gev_mixture_model_object,
#                                                                         alpha = alpha,
#                                                                         data = data,
#                                                                         do.ci = TRUE,
#                                                                         confidence_level = 0.95,
#                                                                         kind = c("geometric", "arithmetic", "harmonic")[1],
#                                                                         iterations = 100)
# 
# results_geometric
# 
# results_arithmetic <- estimate_non_stationary_gev_mixture_model_quantile(ns_gev_mixture_model_object,
#                                                                          alpha = alpha,
#                                                                          data = NULL,
#                                                                          do.ci = TRUE,
#                                                                          confidence_level = 0.95,
#                                                                          kind = c("geometric", "arithmetic", "harmonic")[2],
#                                                                          iterations = 100)
# 
# results_arithmetic
# 
# 
# results_harmonic <- estimate_non_stationary_gev_mixture_model_quantile(ns_gev_mixture_model_object,
#                                                                        alpha = alpha,
#                                                                        data = NULL,
#                                                                        do.ci = TRUE,
#                                                                        confidence_level = 0.95,
#                                                                        kind = c("geometric", "arithmetic", "harmonic")[3],
#                                                                        iterations = 100)
# 
# results_harmonic
# 
# 
# results_geometric <- results_geometric$covariates_1

