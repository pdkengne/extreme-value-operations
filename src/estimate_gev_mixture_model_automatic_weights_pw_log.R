source("./src/calculate_gev_cdf.R")
source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/get_provided_covariates.R")
source("./src/get_knn.R")

estimate_gev_mixture_model_automatic_weights_pw_log <- function(gev_models,
                                                                single_ns_gev_model,
                                                                covariates = NULL,
                                                                k = 50,
                                                                maximum_iterations = 1500, 
                                                                trace = TRUE, 
                                                                use_extremal_index = TRUE,
                                                                use_lower_threshold = FALSE){
  # gev_models: an object associated with a result of the function "estimate_several_gev_models()" or "predict_several_gev_models()"
  # single_ns_gev_model: an object associated with a result of the function "estimate_single_ns_gev_model()"
  # covariates: a named list whose names match the fitted model parameter names,
  # k: the maximum number of nearest neighbors to search
  # maximum_iterations: maximum number of iterations
  # trace: boolean value which indicates whether to print information on the progress of optimization
  # use_extremal_index: a boolean which indicates whether to use the estimates extremal indexes or not
  # use_lower_threshold: a boolean which indicates whether to use threshold associated with the smallest or largest block size
  
  # create an empty output object
  output <- list()
  
  # get the normalized gev parameters
  if (use_extremal_index){
    normalized_gev_parameters <- gev_models$full_normalized_gev_parameters_object
  }
  else{
    normalized_gev_parameters <- gev_models$normalized_gev_parameters_object
  }
  
  shapes <- normalized_gev_parameters$shape_star
  scales <- normalized_gev_parameters$scale_star
  locations <- normalized_gev_parameters$loc_star
  
  # extract the largest data to use
  x <- gev_models$data
  block_sizes <- gev_models$block_sizes
  if (use_lower_threshold){
    block_size <- min(block_sizes)
  }
  else{
    block_size <- max(block_sizes)
  }
  threshold <- find_threshold_associated_with_given_block_size(x, block_size)
  y <- x[x > threshold]
  
  # estimate the empirical distribution function
  provided_covariates <- get_provided_covariates(single_ns_gev_model = single_ns_gev_model, covariates = covariates)
  provided_covariates_knn <- get_knn(data = provided_covariates$used_gev_model_covariates, 
                                     k = k, 
                                     query = provided_covariates$provided_gev_model_covariates)
  x_knn <- x[provided_covariates_knn$id[1, ]]
  Fn <- ecdf(x_knn)
  
  # get the number of gev models
  p <- nrow(normalized_gev_parameters)
  
  # define the constraints on the unknown weights
  lower <- rep(0, 3*p)
  upper <- rep(1, 3*p)
  
  # define an initial vector of weights
  initial_weights <- rep(1, 3*p)/p
  
  # define the error function to optimize
  nlf <- function(w, y){
    weights_shape <- w[1:p]
    weights_scale <- w[(p+1):(2*p)]
    weights_location <- w[(2*p+1):(3*p)]
    
    shape <- sum(weights_shape*shapes)
    scale <- sum(weights_scale*scales)
    location <- sum(weights_location*locations)
    
    theoretical_cdf <- calculate_gev_cdf(q = y, loc = location, scale = scale, shape = shape)
    
    empirical_cdf <- Fn(y)
    
    errors <- (log(theoretical_cdf) - log(empirical_cdf))^2
    
    loss <- sum(errors)
    
    loss
  }
  
  # minimize the error function
  data_A <- c(rep(1, p), rep(0, p), rep(0, p),
               rep(0, p), rep(1, p), rep(0, p),
               rep(0, p), rep(0, p), rep(1, p))
  
  matrix_A <- matrix(data = data_A, nrow = 3, ncol = 3*p, byrow = TRUE)
  
  b <- c(1, 1, 1)
  
  meq <- 3
  
  answer <- BB::BBoptim(par = initial_weights,
                        fn = nlf,
                        y = y,
                        lower = lower,
                        upper = upper,
                        project = "projectLinear",
                        projectArgs = list(A = matrix_A, b = b, meq = meq),
                        control = list(maximize = FALSE, 
                                       trace = trace, 
                                       maxit = maximum_iterations,
                                       # ftol = 1.e-10, 
                                       # gtol = 1e-5, 
                                       # eps = 1e-7,
                                       # maxfeval = 10000,
                                       # triter = 10,
                                       checkGrad = FALSE))
  
  # extract the weights associated with each of the gev model parameters
  automatic_weights_shape <- answer$par[1:p]
  names(automatic_weights_shape) <- block_sizes
  
  automatic_weights_scale <- answer$par[(p+1):(2*p)]
  names(automatic_weights_scale) <- block_sizes
  
  automatic_weights_loc <- answer$par[(2*p+1):(3*p)]
  names(automatic_weights_loc) <- block_sizes
  
  # update the output object
  output[["automatic_weights_shape"]] <- automatic_weights_shape
  output[["automatic_weights_scale"]] <- automatic_weights_scale
  output[["automatic_weights_loc"]] <- automatic_weights_loc
  output[["function_value"]] <- answer$value
  output[["gradient_value"]] <- answer$gradient
  output[["function_reduction"]] <- answer$fn.reduction
  output[["number_iterations"]] <- answer$iter
  output[["convergence"]] <- answer$convergence
  output[["message"]] <- answer$message

  output
}


# # example 1
# 
# source("./src/estimate_several_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/plot_several_standardized_block_maxima_mean.R")
# source("./src/estimate_several_standardized_block_maxima_mean.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# plot_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, equivalent = FALSE)
# plot_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, equivalent = TRUE)
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# equivalent_block_sizes
# 
# rejected_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$rejected))
# rejected_block_sizes
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes)
# 
# results <- estimate_gev_mixture_model_automatic_weights_pw_log(gev_models, trace = TRUE, use_extremal_index = TRUE, use_lower_threshold = FALSE)
# 
# results
# 
# names(results)
# 
# sum(results$automatic_weights_shape)
# sum(results$automatic_weights_scale)
# sum(results$automatic_weights_loc)
# 
# 
# 
# # example 2
# 
# source("./src/estimate_several_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/plot_several_standardized_block_maxima_mean.R")
# source("./src/estimate_several_standardized_block_maxima_mean.R")
# 
# x <- rnorm(n = 1000)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# plot_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, equivalent = FALSE)
# plot_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95, equivalent = TRUE)
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# equivalent_block_sizes
# 
# rejected_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$rejected))
# rejected_block_sizes
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes)
# 
# results <- estimate_gev_mixture_model_automatic_weights_pw_log(gev_models, trace = TRUE, use_extremal_index = TRUE, use_lower_threshold = FALSE)
# 
# results
# 
# names(results)
# 
# sum(results$automatic_weights_shape)
# sum(results$automatic_weights_scale)
# sum(results$automatic_weights_loc)
