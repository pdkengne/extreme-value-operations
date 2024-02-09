source("./src/get_knn.R")
source("./src/make_weights.R")
source("./src/initialize_cluster_data.R")
source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")


calculate_evd_cluster_attractors <- function(x, 
                                             cluster_models,
                                             minimum_cluster_size = 20,
                                             prior_cluster_weights = NULL,
                                             do.ci = FALSE,
                                             confidence_level = 0.95){
  # x:
  # cluster_models:
  # minimum_cluster_size:
  # prior_cluster_weights:
  # do.ci:
  # confidence_level:
  
  nclusters <- length(cluster_models)
  
  if (is.null(prior_cluster_weights)){
    initial_cluster_data <- initialize_cluster_data(x = x, nclusters = nclusters)
    
    prior_cluster_sizes <- sapply(initial_cluster_data, function(data){
      length(data)
    })
    
    prior_cluster_weights <-  make_weights(positives_values = prior_cluster_sizes)
  }
  
  cluster_models_parameters <- lapply(1:nclusters, function(k){
    model <- cluster_models[[k]]
    model$results$par
  })
  
  cluster_attractors_matrix <- sapply(1:nclusters, function(k){
    parameters <- cluster_models_parameters[[k]]
    densities <- calculate_gev_pdf(x = x, 
                                   shape = parameters["shape"], 
                                   scale = parameters["scale"],
                                   loc = parameters["location"])
    
    densities*prior_cluster_weights[k]
  })
  
  cluster_attractors_matrix <- exp(cluster_attractors_matrix)
  mass <- apply(cluster_attractors_matrix, 1, sum)
  cluster_attractors_matrix <- cluster_attractors_matrix/mass
  
  cluster_attractors_frequencies_table <- apply(cluster_attractors_matrix, 1, which.max)
  
  cluster_attractors_frequencies <- sapply(1:nclusters, function(k){
    length(which(cluster_attractors_frequencies_table == k))
  })
  
  selected_cluster_id <- sort(which(cluster_attractors_frequencies >= minimum_cluster_size))
  
  nclusters <- length(selected_cluster_id)
  
  cluster_attractors_frequencies <- cluster_attractors_frequencies[selected_cluster_id]
  
  cluster_attractors_weights <- make_weights(positives_values = cluster_attractors_frequencies)
  
  # extract the selected gev models
  selected_cluster_models <- lapply(selected_cluster_id, function(k){
    model <- cluster_models[[k]]
    model
  })
  
  cluster_models_coefficients <- lapply(selected_cluster_id, function(k){
    model <- cluster_models[[k]]
    model$results$par
  })
  
  cluster_models_coefficients <- do.call(what = rbind, cluster_models_coefficients)
  
  
  if (do.ci){
    cluster_models_coefficients_ci <- lapply(selected_cluster_id, function(k){
      model <- cluster_models[[k]]
      extRemes::ci.fevd(x = model, 
                        type = "parameter",
                        alpha = 1 - confidence_level)
    })
  }
  else {
    cluster_models_coefficients_ci <- NULL
  }
  
  
  
  shapes <- cluster_models_coefficients[, "shape"]
  scales <- cluster_models_coefficients[, "scale"]
  locations <- cluster_models_coefficients[, "location"]
  
  densities <- calculate_gev_mixture_model_pdf(x = x, 
                                               shapes = shapes, 
                                               scales = scales,
                                               locations = locations,
                                               weights = cluster_attractors_weights,
                                               kind = c("geometric", "arithmetic", "harmonic")[2])
  
  loglik <- sum(log(densities))
  
  p <- nclusters
  q <- ncol(cluster_models_coefficients)
  n <- length(x)
  
  aic <- -2*loglik + 2*(q*p + p - 1)
  bic <- -2*loglik + log(n)*(q*p + p - 1)
  
  cluster_information_criteria <- c(aic, bic)
  names(cluster_information_criteria) <- c("AIC", "BIC")
  
  
  cluster_attractors_centers <- sapply(selected_cluster_id, function(k){
    mean(x[which(cluster_attractors_frequencies_table == k)])
  })
  
  cluster_data_list <- lapply(1:nclusters, function(k){
    positions <- which(cluster_attractors_frequencies_table == k)
    
    cluster_data <- x[positions]
    
    cluster_data
  })
  
  output <- list()
  
  # output[["cluster_attractors_matrix"]] <- cluster_attractors_matrix
  output[["cluster_attractors_frequencies"]] <- cluster_attractors_frequencies
  output[["cluster_attractors_weights"]] <- cluster_attractors_weights
  output[["cluster_attractors_centers"]] <- cluster_attractors_centers
  output[["cluster_data_list"]] <- cluster_data_list
  output[["cluster_models"]] <- selected_cluster_models
  output[["cluster_models_coefficients"]] <- cluster_models_coefficients
  output[["cluster_models_coefficients_ci"]] <- cluster_models_coefficients_ci
  output[["loglik"]] <- loglik
  output[["cluster_information_criteria"]] <- cluster_information_criteria
  output[["selected_cluster_id"]] <- selected_cluster_id
  
  output
}


# # example 1
# 
# source("./src/initialize_cluster_data.R")
# source("./src/estimate_evd_cluster_models.R")
# 
# n <- 1000
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(0.6, 1.3), sd = c(0.1, 0.1))
# 
# hist(x, nclass = NULL)
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_cluster_data(x = x, nclusters = 2)
# 
# initial_cluster_data
# 
# 
# cluster_models <- estimate_evd_cluster_models(cluster_data = initial_cluster_data)
# 
# cluster_models
# 
# prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))
# 
# prior_cluster_weights
# 
# cluster_attractors <- calculate_evd_cluster_attractors(x = x,
#                                                        cluster_models = cluster_models,
#                                                        prior_cluster_weights = prior_cluster_weights)
# 
# names(cluster_attractors)
# 
# cluster_attractors$cluster_attractors_centers
# 
# cluster_attractors$cluster_attractors_frequencies
# 
# cluster_attractors$cluster_attractors_weights
# 
# cluster_attractors$cluster_data_list
# 
# 
# # example 2
# 
# source("./src/initialize_cluster_data.R")
# source("./src/generate_gev_mixture_model_sample.R")
# source("./src/estimate_evd_cluster_models.R")
# 
# n <- 1000
# x <- generate_gev_mixture_model_sample(n = n,
#                                        weights = c(1/2, 1/2),
#                                        locations = c(-3, +3),
#                                        scales = c(1, 1),
#                                        shapes = c(-0.01, +0.01),
#                                        kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# hist(x, nclass = 30)
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_cluster_data(x = x, nclusters = 2)
# 
# initial_cluster_data
# 
# 
# cluster_models <- estimate_evd_cluster_models(cluster_data = initial_cluster_data)
# 
# cluster_models
# 
# prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))
# 
# prior_cluster_weights
# 
# cluster_attractors <- calculate_evd_cluster_attractors(x = x,
#                                                        cluster_models = cluster_models,
#                                                        prior_cluster_weights = prior_cluster_weights)
# 
# names(cluster_attractors)
# 
# cluster_attractors$cluster_attractors_centers
# 
# cluster_attractors$cluster_attractors_frequencies
# 
# cluster_attractors$cluster_attractors_weights
# 
# cluster_attractors$cluster_data_list


