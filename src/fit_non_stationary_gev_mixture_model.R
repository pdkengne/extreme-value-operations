# library(extRemes) 
# library(tidyverse)

source("./src/extract_nlargest_sample.R")
source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/get_candidate_block_sizes.R")
source("./src/estimate_several_ns_gev_models.R")
source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
source("./src/estimate_ns_gev_mixture_model_automatic_weights.R")
source("./src/get_several_ns_gev_model_normalized_parameters.R")
source("./src/get_ns_gev_model_parameters.R")
source("./src/estimate_gev_parameters.R")


fit_non_stationary_gev_mixture_model <- function(x,
                                                 data = NULL,
                                                 location.fun = ~ 1,
                                                 scale.fun = ~ 1,
                                                 shape.fun = ~ 1,
                                                 use.phi = FALSE,
                                                 nlargest = Inf,
                                                 block_sizes = NULL,
                                                 minimum_nblocks = 50,
                                                 threshold = NULL,
                                                 confidence_level = 0.95,
                                                 use_extremal_index = TRUE,
                                                 use_uniform_prior = TRUE,
                                                 method = c("MLE", "GMLE")[1]){
  # x: vector of observations
  # nlargest: number of largest values to focus on. Note that the whole vector x is used unless, nlargest != Inf.
  # block_sizes: vector containing the sizes of blocks to consider
  # threshold: lower bound of block maxima
  # use_extremal_index: a boolean which indicates whether to use the estimates extremal indexes or not
  # use_uniform_prior: a boolean which indicates whether to use a uniform prior (TRUE) or a pessimistic prior (FALSE)
  # confidence_level: desired confidence level when extraction equivalent block sizes. 
  #                   Note that this value is ignored if block_sizes != NULL.
  # minimum_nblocks: desired minimum number of blocks. Note that this number is used to infer the largest block size.
  #                  Moreover, this number is ignored if block_sizes != NULL.
  # method: estimation method to use
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # data: dataframe of covariates for linear modeling of the gev model parameters
  
  # create an empty output object
  output <- list()
  
  # get the dataset of all observations
  all_data <- x
  
  # extract the sample of largest values to use
  partial_data <- extract_nlargest_sample(x = all_data, n = nlargest)
  
  # extract covariates associated with the sample of largest values
  if (is.null(data)){
    data <- data.frame("x" = x)
  }
  
  partial_data_positions <- which(x >= min(partial_data))
  
  if (ncol(data) == 1){
    partial_data_covariates <- data.frame("x" = data[partial_data_positions, ])
  } 
  else{
    partial_data_covariates <- data[partial_data_positions, ]
  }
  
  # get candidate block sizes
  if (is.null(block_sizes)){
    candidate_block_sizes <- get_candidate_block_sizes(x = partial_data, 
                                                       threshold = threshold, 
                                                       m = minimum_nblocks)
  }
  else{
    candidate_block_sizes <- block_sizes
  }
  
  # get equivalent block sizes
  equivalent_block_sizes_object <- estimate_several_ns_standardized_block_maxima_mean(x = partial_data, 
                                                                                      block_sizes = candidate_block_sizes,
                                                                                      confidence_level = confidence_level,
                                                                                      data = partial_data_covariates,
                                                                                      location.fun = location.fun,
                                                                                      scale.fun = scale.fun,
                                                                                      shape.fun = shape.fun,
                                                                                      use.phi = use.phi, 
                                                                                      method = method)
  
  equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
  
  # get eventual rejected block sizes from the equivalent ones
  unequivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$rejected))
  
  # find the threshold above which data are used to estimate weights
  threshold <- find_threshold_associated_with_given_block_size(x = partial_data, 
                                                               block_size = min(equivalent_block_sizes))
  
  # estimate several non-stationary gev models associated with the equivalent block sizes
  several_ns_gev_models <- estimate_several_ns_gev_models(x = partial_data, 
                                                          block_sizes = equivalent_block_sizes,
                                                          data = partial_data_covariates,
                                                          location.fun = location.fun,
                                                          scale.fun = scale.fun,
                                                          shape.fun = shape.fun,
                                                          use.phi = use.phi,
                                                          method = method)
  
  # estimate the gev model weights
  automatic_weights_object <- estimate_ns_gev_mixture_model_automatic_weights(gev_models = several_ns_gev_models,
                                                                              x = partial_data,
                                                                              data = partial_data_covariates,
                                                                              use_uniform_prior = use_uniform_prior,
                                                                              use_extremal_index = use_extremal_index)
  # extract the selected block sizes
  selected_block_sizes <- automatic_weights_object$selected_block_sizes
  
  # extract the unselected block sizes
  unselected_block_sizes <- automatic_weights_object$unselected_block_sizes
  
  # extract the labels of selected models
  selected_model_labels <- automatic_weights_object$selected_model_labels
  
  # extract the vector of selected models per observation
  selected_model_per_obs <- automatic_weights_object$selected_model_per_obs
  
  # extract the vector of frequencies associated with the selected models
  frequencies <- as.numeric(table(selected_model_per_obs))
  names(frequencies) <- selected_block_sizes
  
  # extract the vector of weights associated with the selected models
  weights <- automatic_weights_object$weights
  names(weights) <- selected_block_sizes
  
  # extract all non-stationary gev model coefficients associated with the equivalent block sizes
  several_ns_gev_coefficients <- sapply(several_ns_gev_models, function(single_ns_gev_model) 
    single_ns_gev_model$gev_model$results$par)
  
  several_ns_gev_coefficients <- data.frame(t(several_ns_gev_coefficients))
  
  # extract the non-stationary gev model coefficients associated with the selected equivalent block sizes
  selected_ns_gev_coefficients <- several_ns_gev_coefficients[selected_model_labels, ]
  
  # extract all estimated non-stationary gev model objects
  several_ns_gev_models_objects<- lapply(several_ns_gev_models, function(single_ns_gev_model){
    single_ns_gev_model$gev_model
  })
  
  # extract the selected non-stationary gev models
  selected_ns_gev_models <- lapply(selected_model_labels, function(k){
    model <- several_ns_gev_models_objects[[k]]
    model
  })
  
  names(selected_ns_gev_models) <- selected_block_sizes
  
  # extract all extremal indexes associated with the equivalent gev models
  extremal_indexes <- sapply(several_ns_gev_models, function(single_ns_gev_model){
    single_ns_gev_model$extremal_index
  })
  
  # extract the extremal indexes associated with the selected gev models
  extremal_indexes <- extremal_indexes[selected_model_labels]
  
  # calculate the unnormalized gev parameters associated with the selected gev models
  unnormalized_gev_parameters_object <- lapply(selected_ns_gev_models, function(model){
    pars <- get_ns_gev_model_parameters(model, data = NULL)
    as.data.frame(do.call(cbind, pars))
  })
    
  names(unnormalized_gev_parameters_object) <- selected_block_sizes
  
  # extract the selected full non-stationary gev models 
  selected_full_ns_gev_models <- several_ns_gev_models[selected_model_labels]
  
  # calculate the normalized gev parameters associated with the selected gev models
  normalized_gev_parameters_object <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                     data = NULL,
                                                                                     use_extremal_index = FALSE)
  
  # calculate the full normalized gev parameters associated with the selected gev models
  full_normalized_gev_parameters_object <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                                          data = NULL,
                                                                                          use_extremal_index = TRUE)
  
  # extract the negative log-likelihood associated with the selected gev models
  nllh <- sapply(selected_ns_gev_models, function(model){
    res <- summary(model, silent = TRUE)
    res$nllh
  })
  
  # calculate the information criteria, namely aic and bic
  p <- nrow(selected_ns_gev_coefficients)
  q <- ncol(selected_ns_gev_coefficients)
  n <- length(partial_data)
  
  aic <- 2*sum(nllh) + 2*(q*p + p - 1)
  bic <- 2*sum(nllh) + log(n)*(q*p + p - 1)
  
  information_criteria <- c(aic, bic)
  names(information_criteria) <- c("AIC", "BIC")
  
  # calculate model residuals associated with the selected non-stationary gev models
  several_residuals <- lapply(selected_ns_gev_models, function(model){
    residuals <- extRemes::trans(model)
    residuals
  })
  
  # fit stationary gev model to the calculated model residuals
  several_residuals_fit <- lapply(several_residuals, function(single_residuals){
    model <- estimate_gev_parameters(x = single_residuals)
    model
  })
  
  # estimate confidence intervals associated with the residuals models
  several_residuals_diagnosics <- lapply(several_residuals_fit, function(single_residuals_fit){
    confidence_intervals <- extRemes::ci.fevd(x = single_residuals_fit, 
                                              alpha = 0.05, 
                                              type = "parameter")
    confidence_intervals
  })
  
  # update the output object
  output[["threshold"]] <- threshold
  output[["equivalent_block_sizes"]] <- equivalent_block_sizes
  output[["unequivalent_block_sizes"]] <- unequivalent_block_sizes
  output[["selected_block_sizes"]] <- selected_block_sizes
  output[["unselected_block_sizes"]] <- unselected_block_sizes
  
  output[["use_uniform_prior"]] <- use_uniform_prior
  output[["weights"]] <- weights
  output[["frequencies"]] <- frequencies
  
  output[["use_extremal_index"]] <- use_extremal_index
  output[["extremal_indexes"]] <- extremal_indexes
  
  output[["negative_log_likelihoods"]] <- nllh
  output[["information_criteria"]] <- information_criteria
  
  output[["unnormalized_gev_parameters_object"]] <- unnormalized_gev_parameters_object
  output[["normalized_gev_parameters_object"]] <- normalized_gev_parameters_object
  output[["full_normalized_gev_parameters_object"]] <- full_normalized_gev_parameters_object
  
  output[["selected_model_per_obs"]] <- selected_model_per_obs
  output[["all_data_covariates"]] <- data
  output[["partial_data_covariates"]] <- partial_data_covariates
  
  output[["partial_data"]] <- partial_data
  output[["all_data"]] <- all_data
  
  output[["selected_ns_gev_models"]] <- selected_ns_gev_models
  output[["selected_full_ns_gev_models"]] <- selected_full_ns_gev_models

  output[["several_residuals"]] <- several_residuals
  output[["several_residuals_fit"]] <- several_residuals_fit
  output[["several_residuals_diagnosics"]] <- several_residuals_diagnosics

  output
}



# # example 1
# 
# source("./src/calculate_modes.R")
# source("./src/plot_modes.R")
# source("./src/plot_fit_non_stationary_gev_mixture_model.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# source("./src/generate_gev_sample.R")
# 
# #x <- rnorm(n = 3000)
# 
# #x <- rexp(n = 3000)
# 
# n <- 3000
# 
# loc <- 0
# scale <- 1
# shape <- 0.01
# 
# x <- generate_gev_sample(n = n, loc = loc, scale = scale, shape = shape)
# 
# #x <- rnorm(n)
# 
# results <- fit_non_stationary_gev_mixture_model(x = x,
#                                                 data = NULL,
#                                                 nlargest = Inf,
#                                                 block_sizes = NULL,
#                                                 minimum_nblocks = 50,
#                                                 threshold = NULL,
#                                                 confidence_level = 0.95,
#                                                 use_extremal_index = TRUE,
#                                                 use_uniform_prior = TRUE,
#                                                 method = c("MLE", "GMLE")[1])
# 
# names(results)
# 
# # [1] "threshold"                             "equivalent_block_sizes"                "unequivalent_block_sizes"             
# # [4] "selected_block_sizes"                  "unselected_block_sizes"                "use_uniform_prior"                    
# # [7] "weights"                               "frequencies"                           "use_extremal_index"                   
# # [10] "extremal_indexes"                      "negative_log_likelihoods"              "information_criteria"                 
# # [13] "unnormalized_gev_parameters_object"    "normalized_gev_parameters_object"      "full_normalized_gev_parameters_object"
# # [16] "selected_model_per_obs"                "all_data_covariates"                   "partial_data_covariates"              
# # [19] "partial_data"                          "all_data"                              "selected_ns_gev_models"               
# # [22] "selected_full_ns_gev_models"           "several_residuals"                     "several_residuals_fit"                
# # [25] "several_residuals_diagnosics"
# 
# results$threshold
# 
# results$equivalent_block_sizes
# 
# results$unequivalent_block_sizes
# 
# results$selected_block_sizes
# 
# results$unselected_block_sizes
# 
# results$weights
# 
# results$frequencies
# 
# results$use_extremal_index
# 
# results$extremal_indexes
# 
# results$negative_log_likelihoods
# 
# results$information_criteria
# 
# results$unnormalized_gev_parameters_object
# 
# results$normalized_gev_parameters_object
# 
# results$full_normalized_gev_parameters_object
# 
# results$selected_model_per_obs
# 
# results$partial_data
# 
# results$all_data
# 
# results$selected_several_ns_gev_models
# 
# results$several_residuals
# 
# results$several_residuals_fit
# 
# results$several_residuals_diagnosics
# 
# 
# 
# plot_fit_non_stationary_gev_mixture_model(gev_mixture_model_object = results,
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
# 
# 
# # example 2
# 
# source("./src/calculate_modes.R")
# source("./src/plot_modes.R")
# source("./src/plot_fit_non_stationary_gev_mixture_model.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# 
# n <- 3000
# 
# x <- bmixture::rmixnorm(n = n, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))
# 
# results <- fit_non_stationary_gev_mixture_model(x = x,
#                                                 data = NULL,
#                                                 nlargest = 3000,
#                                                 block_sizes = NULL,
#                                                 minimum_nblocks = 50,
#                                                 threshold = NULL,
#                                                 confidence_level = 0.95,
#                                                 use_extremal_index = TRUE,
#                                                 use_uniform_prior = FALSE,
#                                                 method = c("MLE", "GMLE")[1])
# 
# names(results)
# 
# # [1] "threshold"                             "equivalent_block_sizes"                "unequivalent_block_sizes"             
# # [4] "selected_block_sizes"                  "unselected_block_sizes"                "use_uniform_prior"                    
# # [7] "weights"                               "frequencies"                           "use_extremal_index"                   
# # [10] "extremal_indexes"                      "negative_log_likelihoods"              "information_criteria"                 
# # [13] "unnormalized_gev_parameters_object"    "normalized_gev_parameters_object"      "full_normalized_gev_parameters_object"
# # [16] "selected_model_per_obs"                "all_data_covariates"                   "partial_data_covariates"              
# # [19] "partial_data"                          "all_data"                              "selected_ns_gev_models"               
# # [22] "selected_full_ns_gev_models"           "several_residuals"                     "several_residuals_fit"                
# # [25] "several_residuals_diagnosics"
# 
# results$threshold
# 
# results$equivalent_block_sizes
# 
# results$unequivalent_block_sizes
# 
# results$selected_block_sizes
# 
# results$unselected_block_sizes
# 
# results$weights
# 
# results$frequencies
# 
# results$use_extremal_index
# 
# results$extremal_indexes
# 
# results$negative_log_likelihoods
# 
# results$information_criteria
# 
# results$unnormalized_gev_parameters_object
# 
# results$normalized_gev_parameters_object
# 
# results$full_normalized_gev_parameters_object
# 
# results$selected_model_per_obs
# 
# results$partial_data
# 
# results$all_data
# 
# results$selected_several_ns_gev_models
# 
# results$several_residuals
# 
# results$several_residuals_fit
# 
# results$several_residuals_diagnosics
# 
# 
# 
# plot_fit_non_stationary_gev_mixture_model(gev_mixture_model_object = results,
#                                       xlab = "support",
#                                       ylab = "density",
#                                       main = "density plot",
#                                       legend_position = "topright")
# 
# 
# plot_several_ns_standardized_block_maxima_mean(x = x,
#                                             block_sizes = results$selected_block_sizes,
#                                             confidence_level = 0.95,
#                                             equivalent = FALSE,
#                                             method = c("MLE", "GMLE", "Lmoments")[1])
# 
# 
# plot_several_ns_standardized_block_maxima_mean(x = x,
#                                             block_sizes = results$selected_block_sizes,
#                                             confidence_level = 0.95,
#                                             equivalent = TRUE,
#                                             method = c("MLE", "GMLE", "Lmoments")[1])
