source("./src/get_knn.R")
source("./src/make_weights.R")
source("./src/calculate_weibull_mixture_model_pdf.R")


calculate_weibull_cluster_attractors <- function(x, 
                                                 cluster_models,
                                                 minimum_cluster_size = 20,
                                                 prior_cluster_weights = NULL,
                                                 confidence_level = 0.95){
  # x:
  # cluster_models:
  # minimum_cluster_size:
  # prior_cluster_weights:
  # confidence_level:
  
  nclusters <- length(cluster_models)
  
  if (is.null(prior_cluster_weights)){
    prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))
  }
  
  cluster_models_parameters <- lapply(1:nclusters, function(k){
    model <- cluster_models[[k]]
    model$estimate
  })
  
  cluster_attractors_matrix <- sapply(1:nclusters, function(k){
    parameters <- cluster_models_parameters[[k]]
    densities <- dweibull(x = x, 
                          shape = parameters["shape"], 
                          scale = parameters["scale"])
    
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
    model$estimate
  })
  
  cluster_models_coefficients <- do.call(what = rbind, cluster_models_coefficients)
  
  cluster_models_coefficients_ci <- lapply(selected_cluster_id, function(k){
    model <- cluster_models[[k]]
    confint(object = model, level = confidence_level)
  })
    
  locations <- cluster_models_coefficients[, "shape"]
  scales <- cluster_models_coefficients[, "scale"]
  
  densities <- calculate_weibull_mixture_model_pdf(x = x, 
                                                   locations = locations, 
                                                   scales = scales, 
                                                   weights = cluster_attractors_weights,
                                                   kind = c("geometric", "arithmetic")[2])
  
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
  
  data <- data.frame(x = x)
  
  cluster_data_list <- lapply(1:nclusters, function(k){
    center <- cluster_attractors_centers[k]

    size <- ifelse(test = nclusters == 1, 
                   yes = ceiling(n - 1), 
                   no = ceiling(cluster_attractors_frequencies[k]))
    
    cluster_data_object <- get_knn(data = data, k = size, query = center)
    cluster_data <- x[cluster_data_object$id[1, ]]
    
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
# source("./src/calculate_weibull_mixture_model_cdf.R")
# 
# n <- 1000
# x <- mixR::rmixweibull(n = n, pi = c(1/2, 1/2), mu = c(0.6, 1.3), sd = c(0.1, 0.1))
# 
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_cluster_data(x = x, nclusters = 2)
# 
# initial_cluster_data
# 
# 
# cluster_models <- estimate_weibull_cluster_models(cluster_data = initial_cluster_data)
# 
# cluster_models
# 
# prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))
# 
# prior_cluster_weights
# 
# cluster_attractors <- calculate_weibull_cluster_attractors(x = x,
#                                                            cluster_models = cluster_models,
#                                                            prior_cluster_weights = prior_cluster_weights)
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



