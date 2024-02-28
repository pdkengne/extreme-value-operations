# library(BB)

source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/calculate_gev_pdf.R")

estimate_gev_mixture_model_automatic_weights <- function(gev_models,
                                                         use_uniform_prior = TRUE,
                                                         use_extremal_index = TRUE){
  # gev_models: an object associated with a result of the function "estimate_several_gev_models()"
  # use_uniform_prior: a boolean which indicates whether to use a uniform prior (TRUE) or a pessimistic prior (FALSE)
  # use_extremal_index: a boolean which indicates whether to use the estimates extremal indexes or not
  
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
  
  # extract the vector of block sizes
  block_sizes <- gev_models$block_sizes
  
  # extract the vector of observations
  x <- gev_models$data
  
  # find the threshold to use
  threshold <- find_threshold_associated_with_given_block_size(x = x, 
                                                               block_size = min(block_sizes))
  
  # extract the largest data to use
  x <- x[x > threshold]
  
  # # calculate the prior probability w.r.t. every gev model
  # if (use_uniform_prior){
  #   unnormalized_prior <- block_sizes/block_sizes
  #   prior <- unnormalized_prior/sum(unnormalized_prior)
  # } 
  # else{
  #   unnormalized_prior <- exp(shapes)
  #   prior <- unnormalized_prior/sum(unnormalized_prior)
  # }
  # 
  # # calculate the posterior probability w.r.t. every gev model
  # normalized_posterior <- sapply(x, function(obs){
  #   unnormalized_posterior <- sapply(1:length(block_sizes), function(k){
  #     likelihood <- calculate_gev_pdf(x = obs,
  #                                     loc = locations[k],
  #                                     scale = scales[k],
  #                                     shape = shapes[k])
  #     # likelihood*prior[k]
  #     exp(likelihood*prior[k])
  #   })
  #   unnormalized_posterior/sum(unnormalized_posterior)
  # })
  # 
  # # check the consistency of the estimated gev models
  # posterior_nb_na <- sum(is.na(normalized_posterior))
  # if (posterior_nb_na != 0){
  #   stop("Sorry, at least one of the estimated GEV models is inconsistent!")
  # }
  # 
  # # calculate the vector of weights
  # if (class(normalized_posterior)[1] == "numeric"){
  #   selected_model_per_obs <- normalized_posterior
  # }
  # else{
  #   selected_model_per_obs <- apply(normalized_posterior, 2, which.max)
  # }
  
  # to be considered (in all situations)
  set.seed(length(x))
  selected_model_per_obs <- sample(x = 1:length(block_sizes), size = length(x), replace = TRUE)
  
  selected_model_freq <- table(selected_model_per_obs)
  selected_model_labels <- as.numeric(names(selected_model_freq))
  weights <- as.numeric(prop.table(selected_model_freq))
  
  # get the vector of selected block sizes
  selected_block_sizes <- block_sizes[selected_model_labels]
  
  # extract as factor the vector of selected models per observation
  selected_model_per_obs <- factor(selected_model_per_obs)
  levels(selected_model_per_obs) <- selected_block_sizes
  
  # get the vector of unselected block sizes
  unselected_block_sizes <- block_sizes[-selected_model_labels]
  
  # update the output object
  output[["threshold"]] <- threshold
  output[["selected_model_per_obs"]] <- selected_model_per_obs
  output[["unselected_block_sizes"]] <- unselected_block_sizes
  output[["selected_block_sizes"]] <- selected_block_sizes
  output[["selected_model_labels"]] <- selected_model_labels
  output[["weights"]] <- weights
  
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
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes)
# 
# results <- estimate_gev_mixture_model_automatic_weights(gev_models = gev_models,
#                                                         use_uniform_prior = TRUE,
#                                                         use_extremal_index = TRUE)
# 
# names(results)
# 
# # [1] "threshold"              "selected_model_per_obs" "unselected_block_sizes" "selected_block_sizes"   "selected_model_labels"
# # [6] "weights"
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
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes)
# 
# results <- estimate_gev_mixture_model_automatic_weights(gev_models = gev_models,
#                                                         use_uniform_prior = TRUE,
#                                                         use_extremal_index = TRUE)
# 
# names(results)
