# library(dplyr)

source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/calculate_gev_pdf.R")
source("./src/get_several_ns_gev_model_normalized_parameters.R")


estimate_ns_gev_mixture_model_automatic_weights <- function(gev_models,
                                                            x,
                                                            data,
                                                            use_uniform_prior = TRUE,
                                                            use_extremal_index = TRUE){
  # gev_models: an object associated with a result of the function "estimate_several_ns_gev_models()"
  # x: vector of observations
  # data: dataframe of covariates for linear modeling of the gev model parameters
  # use_uniform_prior: a boolean which indicates whether to use a uniform prior (TRUE) or a pessimistic prior (FALSE)
  # use_extremal_index: a boolean which indicates whether to use the estimates extremal indexes or not
  
  # create an empty output object
  output <- list()
  
  # extract the vector of block sizes
  block_sizes <- sapply(gev_models, function(single_ns_gev_model){
    single_ns_gev_model$block_size
  })
  
  # find the threshold to use
  threshold <- find_threshold_associated_with_given_block_size(x = x, block_size = min(block_sizes))
  
  # extract the largest data to use
  y <- 1:length(x)
  index <- y[x > threshold]
  x <- x[index]
  data <- dplyr::slice(data, index)
  
  # get the normalized gev parameters
  normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = gev_models, 
                                                                              data = data, 
                                                                              use_extremal_index = use_extremal_index,
                                                                              normalize_parameters = TRUE)

  # calculate the prior probability w.r.t. every gev model
  if (use_uniform_prior){
    unnormalized_prior <- block_sizes/block_sizes
    prior <- unnormalized_prior/sum(unnormalized_prior)
  } 
  else{
    highest_shapes <- sapply(1:length(normalized_gev_parameters), function(k){
      shape <- max(normalized_gev_parameters[[k]]$shape)
      shape
    })
    unnormalized_prior <- exp(highest_shapes)
    prior <- unnormalized_prior/sum(unnormalized_prior)
  }
  
  # calculate the posterior probability w.r.t. every gev model
  normalized_posterior <- sapply(1:length(x), function(i){
    obs <- x[i]
    unnormalized_posterior <- sapply(1:length(block_sizes), function(k){
      parameters <- normalized_gev_parameters[[k]]
      
      location <- parameters$location[i]
      scale <- parameters$scale[i]
      shape <- parameters$shape[i]
      
      likelihood <- calculate_gev_pdf(x = obs,
                                      loc = location,
                                      scale = scale,
                                      shape = shape)
      # likelihood*prior[k]
      exp(likelihood*prior[k])
    })
    unnormalized_posterior/sum(unnormalized_posterior)
  })
  
  # check the consistency of the estimated gev models
  posterior_nb_na <- sum(is.na(normalized_posterior))
  if (posterior_nb_na != 0){
    stop("Sorry, at least one of the estimated GEV models is inconsistent!")
  }
  
  # calculate the vector of weights
  if (class(normalized_posterior)[1] == "numeric"){
    selected_model_per_obs <- normalized_posterior
  }
  else{
    selected_model_per_obs <- apply(normalized_posterior, 2, which.max)
  }
  
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
# source("./src/estimate_several_ns_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
# 
# n <- 1000
# 
# x <- rnorm(n = 1000)
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# block_sizes
# 
# plot_several_ns_standardized_block_maxima_mean(x,
#                                                block_sizes,
#                                                confidence_level = 0.95,
#                                                equivalent = FALSE,
#                                                data = data,
#                                                location.fun = ~ .,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# 
# plot_several_ns_standardized_block_maxima_mean(x,
#                                                block_sizes,
#                                                confidence_level = 0.95,
#                                                equivalent = TRUE,
#                                                data = data,
#                                                location.fun = ~ .,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# 
# equivalent_block_sizes_object<- estimate_several_ns_standardized_block_maxima_mean(x,
#                                                                                    block_sizes,
#                                                                                    confidence_level = 0.95,
#                                                                                    data = data,
#                                                                                    location.fun = ~ .,
#                                                                                    scale.fun = ~ .,
#                                                                                    shape.fun = ~1,
#                                                                                    use.phi = TRUE,
#                                                                                    type = c("GEV", "Gumbel")[1],
#                                                                                    method = c("MLE", "GMLE")[2])
# 
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# several_ns_gev_models <- estimate_several_ns_gev_models(x = x,
#                                                         block_sizes = equivalent_block_sizes,
#                                                         data = data,
#                                                         location.fun = ~ .,
#                                                         scale.fun = ~ .,
#                                                         shape.fun = ~ 1,
#                                                         use.phi = TRUE,
#                                                         type = c("GEV", "Gumbel")[1],
#                                                         method = c("MLE", "GMLE")[2])
# 
# results <- estimate_ns_gev_mixture_model_automatic_weights(gev_models = several_ns_gev_models,
#                                                            x = x,
#                                                            data = data,
#                                                            use_uniform_prior = TRUE,
#                                                            use_extremal_index = TRUE)
# 
# names(results)
# 
# # [1] "threshold"              "selected_model_per_obs" "unselected_block_sizes" "selected_block_sizes"   "selected_model_labels"
# # [6] "weights"
# 
# results$weights
# 
# results$selected_block_sizes
# 
# results$unselected_block_sizes
# 
# 
# # example 2
# 
# source("./src/estimate_several_ns_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
# 
# n <- 1000
# 
# x <- rnorm(n = 1000)
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# block_sizes
# 
# plot_several_ns_standardized_block_maxima_mean(x,
#                                                block_sizes,
#                                                confidence_level = 0.95,
#                                                equivalent = FALSE,
#                                                data = NULL,
#                                                location.fun = ~ 1,
#                                                scale.fun = ~ 1,
#                                                shape.fun = ~ 1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# 
# plot_several_ns_standardized_block_maxima_mean(x,
#                                                block_sizes,
#                                                confidence_level = 0.95,
#                                                equivalent = TRUE,
#                                                data = NULL,
#                                                location.fun = ~ 1,
#                                                scale.fun = ~ 1,
#                                                shape.fun = ~ 1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# 
# equivalent_block_sizes_object<- estimate_several_ns_standardized_block_maxima_mean(x,
#                                                                                    block_sizes,
#                                                                                    confidence_level = 0.95,
#                                                                                    data = NULL,
#                                                                                    location.fun = ~ 1,
#                                                                                    scale.fun = ~ 1,
#                                                                                    shape.fun = ~ 1,
#                                                                                    use.phi = TRUE,
#                                                                                    type = c("GEV", "Gumbel")[1],
#                                                                                    method = c("MLE", "GMLE")[2])
# 
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# several_ns_gev_models <- estimate_several_ns_gev_models(x = x,
#                                                         block_sizes = equivalent_block_sizes,
#                                                         data = NULL,
#                                                         location.fun = ~ 1,
#                                                         scale.fun = ~ 1,
#                                                         shape.fun = ~ 1,
#                                                         use.phi = TRUE,
#                                                         type = c("GEV", "Gumbel")[1],
#                                                         method = c("MLE", "GMLE")[2])
# 
# results <- estimate_ns_gev_mixture_model_automatic_weights(gev_models = several_ns_gev_models,
#                                                            x = x,
#                                                            data = data,
#                                                            use_uniform_prior = TRUE,
#                                                            use_extremal_index = TRUE)
# 
# names(results)
# 
# # [1] "threshold"              "selected_model_per_obs" "unselected_block_sizes" "selected_block_sizes"   "selected_model_labels"
# # [6] "weights"
# 
# results$weights
# 
# results$selected_block_sizes
# 
# results$unselected_block_sizes
