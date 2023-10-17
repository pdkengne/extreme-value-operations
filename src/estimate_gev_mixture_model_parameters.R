source("./src/extract_nlargest_sample.R")
source("./src/get_candidate_block_sizes.R")
source("./src/estimate_several_gev_models.R")
source("./src/estimate_gev_mixture_model_identic_weights.R")
source("./src/estimate_gev_mixture_model_pessimistic_weights.R")
source("./src/estimate_several_standardized_block_maxima_mean.R")
source("./src/estimate_gev_mixture_model_automatic_weights_mw.R")
source("./src/estimate_gev_mixture_model_automatic_weights_pw.R")
source("./src/estimate_gev_mixture_model_automatic_weights_mw_log.R")
source("./src/estimate_gev_mixture_model_automatic_weights_pw_log.R")

estimate_gev_mixture_model_parameters <- function(x, 
                                                  block_sizes = NULL,
                                                  minimum_nblocks = 50,
                                                  threshold = NULL,
                                                  nlargest = Inf,
                                                  confidence_level = 0.95,
                                                  use_extremal_index = TRUE,
                                                  use_lower_threshold = FALSE,
                                                  maximum_iterations = 1500,
                                                  log_mv = TRUE,
                                                  log_pw = TRUE,
                                                  trace = TRUE,
                                                  method = c("MLE", "GMLE", "Lmoments")[1]){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # trace: boolean value which indicates whether to print information on the progress of optimization
  # log_mv: a boolean which indicates whether the model wise loss function is at logarithmic scale or not
  # log_pw: a boolean which indicates whether the parameter wise loss function is at logarithmic scale or not
  # nlargest: number of largest values to focus on. Note that the whole vector x is used unless, nlargest != Inf.
  # maximum_iterations: maximum number of iterations
  # threshold: lower bound of block maxima
  # use_extremal_index: a boolean which indicates whether to use the estimates extremal indexes or not
  # use_lower_threshold: a boolean which indicates whether to use threshold associated with the smallest or largest block size
  # confidence_level: desired confidence level when extraction equivalent block sizes. 
  #                   Note that this value is ignored if block_sizes != NULL.
  # minimum_nblocks: desired minimum number of blocks. Note that this number is used to infer the largest block size.
  #                  Moreover, this number is ignored if block_sizes != NULL.
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  # extract the sample of largest values to use
  data_largest <- extract_nlargest_sample(x, n = nlargest)
  
  # get candidate block sizes
  if (is.null(block_sizes)){
    block_sizes <- get_candidate_block_sizes(x = data_largest, threshold = threshold, m = minimum_nblocks)
  }
  
  # get equivalent block sizes
  equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x = data_largest, 
                                                                                  block_sizes = block_sizes, 
                                                                                  confidence_level = confidence_level,
                                                                                  method = method)
  
  equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
  
  # get eventual rejected block sizes
  rejected_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$rejected))
  
  # estimate several gev models associated with the equivalent block sizes
  gev_models <- estimate_several_gev_models(x = data_largest, 
                                            block_sizes = equivalent_block_sizes,
                                            method = method)
  
  # get all gev models
  gev_models_object <- gev_models$gev_models_object
  
  # get the block maxima
  block_maxima_object <- gev_models$block_maxima_object
  
  # get the block maxima indexes
  block_maxima_indexes_object <- gev_models$block_maxima_indexes_object
  
  # get the extremal indexes
  extremal_indexes <- gev_models$extremal_indexes
  
  # estimate the identic weights
  identic_weights_mw <- estimate_gev_mixture_model_identic_weights(gev_models = gev_models,
                                                                   use_extremal_index = use_extremal_index)
  
  # estimate the pessimistic weights
  pessimistic_weights_object <- estimate_gev_mixture_model_pessimistic_weights(gev_models = gev_models,
                                                                               use_extremal_index = use_extremal_index)
  pessimistic_weights_mw <- pessimistic_weights_object$pessimistic_weights
  
  # estimate model wise automatic weights 
  if (log_mv){
    automatic_weights_mw_object <- estimate_gev_mixture_model_automatic_weights_mw_log(gev_models = gev_models,
                                                                                       maximum_iterations = maximum_iterations, 
                                                                                       trace = trace,
                                                                                       use_extremal_index = use_extremal_index,
                                                                                       use_lower_threshold = use_lower_threshold)
  }
  else{
    automatic_weights_mw_object <- estimate_gev_mixture_model_automatic_weights_mw(gev_models = gev_models,
                                                                                   maximum_iterations = maximum_iterations, 
                                                                                   trace = trace,
                                                                                   use_extremal_index = use_extremal_index,
                                                                                   use_lower_threshold = use_lower_threshold)
  }
  automatic_weights_mw <- automatic_weights_mw_object$automatic_weights
  automatic_weights_mw_statistics <- list(function_value = automatic_weights_mw_object$function_value,
                                          gradient_value = automatic_weights_mw_object$gradient_value,
                                          function_reduction = automatic_weights_mw_object$function_reduction,
                                          number_iterations = automatic_weights_mw_object$number_iterations,
                                          convergence = automatic_weights_mw_object$convergence,
                                          message = automatic_weights_mw_object$message)
  
  # estimate parameter wise automatic weights
  if (log_pw){
    automatic_weights_pw_object <- estimate_gev_mixture_model_automatic_weights_pw_log(gev_models = gev_models,
                                                                                       maximum_iterations = maximum_iterations, 
                                                                                       trace = trace,
                                                                                       use_extremal_index = use_extremal_index,
                                                                                       use_lower_threshold = use_lower_threshold)
  }
  else{
    automatic_weights_pw_object <- estimate_gev_mixture_model_automatic_weights_pw(gev_models = gev_models,
                                                                                   maximum_iterations = maximum_iterations, 
                                                                                   trace = trace,
                                                                                   use_extremal_index = use_extremal_index,
                                                                                   use_lower_threshold = use_lower_threshold)
  }
  automatic_weights_pw_statistics <- list(function_value = automatic_weights_pw_object$function_value,
                                          gradient_value = automatic_weights_pw_object$gradient_value,
                                          function_reduction = automatic_weights_pw_object$function_reduction,
                                          number_iterations = automatic_weights_pw_object$number_iterations,
                                          convergence = automatic_weights_pw_object$convergence,
                                          message = automatic_weights_pw_object$message)
  
  # get the normalized gev model parameters
  normalized_gev_parameters_object <- gev_models$normalized_gev_parameters_object
  
  # get the normalized gev model parameters
  full_normalized_gev_parameters_object <- gev_models$full_normalized_gev_parameters_object
  
  # calculate the identic weighted normalized gev model parameters
  identic_weighted_normalized_gev_parameters_object <- apply(normalized_gev_parameters_object, 2, mean)
  
  # calculate the pessimistic weighted normalized gev model parameters
  pessimistic_weights_pw_shape <- pessimistic_weights_object$pessimistic_weights_shape
  pessimistic_weights_pw_scale <- pessimistic_weights_object$pessimistic_weights_scale
  pessimistic_weights_pw_loc <- pessimistic_weights_object$pessimistic_weights_loc
  pessimistic_weighted_normalized_gev_parameters_object <- c(sum(pessimistic_weights_pw_loc*normalized_gev_parameters_object$loc_star),
                                                             sum(pessimistic_weights_pw_scale*normalized_gev_parameters_object$scale_star),
                                                             sum(pessimistic_weights_pw_shape*normalized_gev_parameters_object$shape_star))
  
  # calculate the automatic weighted normalized gev model parameters
  automatic_weights_pw_shape <- automatic_weights_pw_object$automatic_weights_shape
  automatic_weights_pw_scale <- automatic_weights_pw_object$automatic_weights_scale
  automatic_weights_pw_loc <- automatic_weights_pw_object$automatic_weights_loc
  automatic_weighted_normalized_gev_parameters_object <- c(sum(automatic_weights_pw_loc*normalized_gev_parameters_object$loc_star),
                                                           sum(automatic_weights_pw_scale*normalized_gev_parameters_object$scale_star),
                                                           sum(automatic_weights_pw_shape*normalized_gev_parameters_object$shape_star))
  
  # calculate the weighted normalized gev model parameters
  weighted_normalized_gev_parameters_object <- data.frame(rbind(identic_weighted_normalized_gev_parameters_object,
                                                                pessimistic_weighted_normalized_gev_parameters_object,
                                                                automatic_weighted_normalized_gev_parameters_object))
  names(weighted_normalized_gev_parameters_object) <- names(normalized_gev_parameters_object)
  rownames(weighted_normalized_gev_parameters_object) <- c("identic_weights", "pessimistic_weights", "automatic_weights")
  
  # update the output object
  output[["data"]] <- x
  output[["data_largest"]] <- data_largest
  
  output[["use_lower_threshold"]] <- use_lower_threshold
  
  
  output[["block_sizes"]] <- block_sizes
  output[["equivalent_block_sizes"]] <- equivalent_block_sizes
  output[["rejected_block_sizes"]] <- rejected_block_sizes
  output[["block_maxima_object"]] <- block_maxima_object
  output[["block_maxima_indexes_object"]] <- block_maxima_indexes_object
  
  output[["gev_models_object"]] <- gev_models_object
  output[["extremal_indexes"]] <- extremal_indexes
  output[["use_extremal_index"]] <- use_extremal_index
  
  output[["normalized_gev_parameters_object"]] <- normalized_gev_parameters_object
  output[["full_normalized_gev_parameters_object"]] <- full_normalized_gev_parameters_object
  output[["weighted_normalized_gev_parameters_object"]] <- weighted_normalized_gev_parameters_object
  
  output[["identic_weights_mw"]] <- identic_weights_mw
  output[["pessimistic_weights_mw"]] <- pessimistic_weights_mw
  
  output[["pessimistic_weights_pw_shape"]] <- pessimistic_weights_pw_shape
  output[["pessimistic_weights_pw_scale"]] <- pessimistic_weights_pw_scale
  output[["pessimistic_weights_pw_loc"]] <- pessimistic_weights_pw_loc
  
  output[["automatic_weights_mw"]] <- automatic_weights_mw
  output[["automatic_weights_mw_statistics"]] <- automatic_weights_mw_statistics
  
  output[["automatic_weights_pw_shape"]] <- automatic_weights_pw_shape
  output[["automatic_weights_pw_scale"]] <- automatic_weights_pw_scale
  output[["automatic_weights_pw_loc"]] <- automatic_weights_pw_loc
  output[["automatic_weights_pw_statistics"]] <- automatic_weights_pw_statistics
  
  output 
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/plot_several_standardized_block_maxima_mean.R")
# 
# n <- 10000
# nlargest <- 1000
# 
# # x <- rnorm(n = n)
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# results <- estimate_gev_mixture_model_parameters(x,
#                                                  block_sizes = NULL,
#                                                  minimum_nblocks = 50,
#                                                  threshold = NULL,
#                                                  nlargest = nlargest,
#                                                  confidence_level = 0.95,
#                                                  use_extremal_index = TRUE,
#                                                  use_lower_threshold = FALSE,
#                                                  maximum_iterations = 1500,
#                                                  log_mv = TRUE,
#                                                  log_pw = TRUE,
#                                                  trace = TRUE,
#                                                  method = c("MLE", "GMLE", "Lmoments")[1])
# 
# #results
# names(results)
# 
# # "data"                                      "data_largest"                              "use_lower_threshold"                       "block_sizes"                              
# # "equivalent_block_sizes"                    "rejected_block_sizes"                      "block_maxima_object"                       "block_maxima_indexes_object"              
# # "gev_models_object"                         "extremal_indexes"                          "normalized_gev_parameters_object"          "full_normalized_gev_parameters_object"    
# # "weighted_normalized_gev_parameters_object" "identic_weights_mw"                        "pessimistic_weights_mw"                    "pessimistic_weights_pw_shape"             
# # "pessimistic_weights_pw_scale"              "pessimistic_weights_pw_loc"                "automatic_weights_mw"                      "automatic_weights_mw_statistics"          
# # "automatic_weights_pw_shape"                "automatic_weights_pw_scale"                "automatic_weights_pw_loc"                  "automatic_weights_pw_statistics"   
# 
# # get the block sizes
# results$block_sizes
# 
# # get the extremal indexes
# results$extremal_indexes
# 
# # get the normalized gev parameters
# results$normalized_gev_parameters_object
# 
# # get the full normalized gev parameters
# results$full_normalized_gev_parameters_object
# 
# # get model wise automatic weights
# results$automatic_weights_mw
# 
# # get the weighted normalized gev parameters
# results$weighted_normalized_gev_parameters_object
# 
# # get the statistics about the estimation of weights
# results$automatic_weights_mw_statistics
# results$automatic_weights_pw_statistics
# 
# # plot the mean standardized block maxima
# plot_several_standardized_block_maxima_mean(x = results$data_largest,
#                                             block_sizes = results$block_sizes,
#                                             confidence_level = 0.95,
#                                             equivalent = FALSE,
#                                             xlab = "Block Sizes",
#                                             ylab = "Estimated Values",
#                                             main = "Mean Standardized Block Maxima Plot")
# 
# # plot the mean standardized block maxima (only equivalent models)
# plot_several_standardized_block_maxima_mean(x = results$data_largest,
#                                             block_sizes = results$block_sizes,
#                                             confidence_level = 0.95,
#                                             equivalent = TRUE,
#                                             xlab = "Block Sizes",
#                                             ylab = "Estimated Values",
#                                             main = "Mean Standardized Block Maxima Plot")
# # get the rejected block sizes
# results$rejected_block_sizes

