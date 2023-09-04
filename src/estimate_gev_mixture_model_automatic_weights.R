# library(BB)

source("./src/calculate_gev_cdf.R")
source("./src/calculate_gev_mixture_model_cdf.R")
source("./src/estimate_gev_mixture_model_pessimistic_weights.R")
source("./src/estimate_gev_mixture_model_identic_weights.R")
source("./src/find_threshold_associated_with_given_block_size.R")

estimate_gev_mixture_model_automatic_weights <- function(gev_models, trace = TRUE){
  # gev_models: an object associated with a result of the function "estimate_several_gev_models()"
  # trace: boolean value which indicates whether to print information on the progress of optimization
  
  # get the normalized gev parameters
  normalized_gev_parameters <- gev_models$normalized_gev_parameters_object
  shapes <- normalized_gev_parameters$shape_star
  scales <- normalized_gev_parameters$scale_star
  locations <- normalized_gev_parameters$loc_star
  
  # extract the largest data to use
  x <- gev_models$data
  block_size_max <- max(gev_models$block_sizes)
  threshold_max <- find_threshold_associated_with_given_block_size(x, block_size_max)
  y <- x[x > threshold_max]
  
  # estimate the empirical distribution function
  Fn <- ecdf(x)
  
  # get the number of gev models
  p <- nrow(normalized_gev_parameters)
  
  # define the constraints on the unknown weights
  lower <- rep(0, p)
  upper <- rep(1, p)
  
  # define some initial vectors of weights
  initial_weights_object <- estimate_gev_mixture_model_pessimistic_weights(gev_models)
  
  initial_weights_pessimistic_weights_shape <- initial_weights_object$pessimistic_weights_shape
  initial_weights_pessimistic_weights_scale <- initial_weights_object$pessimistic_weights_scale
  initial_weights_pessimistic_weights_loc <- initial_weights_object$pessimistic_weights_loc
  
  initial_weights_identic <- estimate_gev_mixture_model_identic_weights(gev_models)
  
  initial_weights_matrix <- rbind(initial_weights_pessimistic_weights_shape,
                                  initial_weights_pessimistic_weights_scale,
                                  initial_weights_pessimistic_weights_loc,
                                  initial_weights_identic)
  
  v <- runif(n = p)
  weights_1 <- v/sum(v)
  v <- runif(n = p)
  weights_2 <- v/sum(v)
  v <- runif(n = p)
  weights_3 <- v/sum(v)
  
  # initial_weights_matrix <- rbind(weights_1, weights_2, weights_3)
  
  # define the error function to optimize
  nlf <- function(w, y){
    theoretical_cdf <- calculate_gev_mixture_model_cdf(q = y, locations, scales, shapes, weights = w)
    
    empirical_cdf <- Fn(y)
    
    errors <- (theoretical_cdf - empirical_cdf)^2
    
    loss <- sum(errors)
    
    loss
  }
  
  # define the gradient of error function to optimize
  
  nlf_gradient <- function(w, y){
    theoretical_cdf <- calculate_gev_mixture_model_cdf(q = y, locations, scales, shapes, weights = w)
    
    empirical_cdf <- Fn(y)
    
    errors <- theoretical_cdf - empirical_cdf
    
    gradient_object <- sapply(1:p, function(j) errors*log(calculate_gev_cdf(q = y, 
                                                                            loc = locations[j], 
                                                                            scale = scales[j], 
                                                                            shape = shapes[j])))
    
    gradient <- 2*apply(gradient_object, 2, sum)
    
    gradient
  }
  
  # minimize the error function
  answer <- BB::BBoptim(par = initial_weights_identic,
                        fn = nlf,
                        gr = nlf_gradient,
                        y = y,
                        lower = lower,
                        upper = upper,
                        project = "projectLinear",
                        projectArgs=list(A = matrix(1, nrow = 1, ncol = p), b = 1, meq = 1),
                        control = list(maximize = FALSE, trace = trace, checkGrad = FALSE))
  
  # # minimize the error function
  # answer <- BB::multiStart(par = initial_weights_matrix,
  #                         fn = nlf,
  #                         gr = nlf_gradient,
  #                         action="optimize",
  #                         y = y,
  #                         lower = lower,
  #                         upper = upper,
  #                         project = "projectLinear",
  #                         projectArgs=list(A = matrix(1, nrow = 1, ncol = p), b = 1, meq = 1),
  #                         control = list(maximize = FALSE, trace = trace, checkGrad = FALSE))
  
  answer
}


# # example 1
# 
# source("./src/estimate_several_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
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
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes, nsloc = NULL)
# 
# results <- estimate_gev_mixture_model_automatic_weights(gev_models, trace = TRUE)
# 
# results
# 
# sum(results$par)
# 
# 
# # example 2
# 
# source("./src/estimate_several_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/estimate_several_standardized_block_maxima_mean.R")
# 
# x <- rnorm(n = 1000)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 100)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes, nsloc = NULL)
# 
# results <- estimate_gev_mixture_model_automatic_weights(gev_models, trace = TRUE)
# 
# results
# 
# sum(results$par)
