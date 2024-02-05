source("./src/get_knn.R")
source("./src/make_weights.R")


calculate_normal_cluster_attractors <- function(x, cluster_models, prior_cluster_weights){
  # x:
  # cluster_models:
  # prior_cluster_weights:
  
  nclusters <- length(cluster_models)
  
  cluster_models_parameters <- lapply(1:nclusters, function(k){
    model <- cluster_models[[k]]
    model$estimate
  })
  
  cluster_models_coefficients <- do.call(what = rbind, cluster_models_parameters)
  
  nllh <- sapply(1:nclusters, function(k){
    model <- cluster_models[[k]]
    -1*model$loglik
  })
  
  names(nllh) <- 1:nclusters
  
  p <- nclusters
  q <- ncol(cluster_models_coefficients)
  n <- length(x)
  
  aic <- 2*sum(nllh) + 2*(q*p + p - 1)
  bic <- 2*sum(nllh) + log(n)*(q*p + p - 1)
  
  cluster_information_criteria <- c(aic, bic)
  names(cluster_information_criteria) <- c("AIC", "BIC")
  
  cluster_attractors_matrix <- sapply(1:nclusters, function(k){
    parameters <- cluster_models_parameters[[k]]
    densities <- dnorm(x = x, 
                       mean = parameters["mean"], 
                       sd = parameters["sd"])
    
    densities*prior_cluster_weights[k]
  })
  
  cluster_attractors_matrix <- exp(cluster_attractors_matrix)
  mass <- apply(cluster_attractors_matrix, 1, sum)
  cluster_attractors_matrix <- cluster_attractors_matrix/mass
  
  cluster_attractors_frequencies_table <- apply(cluster_attractors_matrix, 1, which.max)
  cluster_attractors_frequencies <- sapply(1:nclusters, function(k){
    length(which(cluster_attractors_frequencies_table == k))
  })
  
  
  cluster_attractors_weights <- make_weights(positives_values = cluster_attractors_frequencies)
  
  cluster_attractors_centers <- sapply(1:nclusters, function(k){
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
  output[["cluster_models"]] <- cluster_models
  output[["cluster_models_coefficients"]] <- cluster_models_coefficients
  output[["cluster_information_criteria"]] <- cluster_information_criteria
  
  output
}


cluster_models_coefficients

# # example 1
# 
# source("./src/initialize_cluster_data.R")
# source("./src/calculate_normal_mixture_model_cdf.R")
# 
# n <- 100
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(-10, +10), sd = c(1, 1))
# 
# 
# nclusters <- 3
# 
# initial_cluster_data <- initialize_cluster_data(x = x, nclusters = 3)
# 
# initial_cluster_data
# 
# 
# cluster_models <- estimate_normal_cluster_models(cluster_data = initial_cluster_data)
# 
# cluster_models
# 
# prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))
# 
# prior_cluster_weights
# 
# cluster_attractors <- calculate_normal_cluster_attractors(x = x, 
#                                                           cluster_models = cluster_models, 
#                                                           prior_cluster_weights = prior_cluster_weights)
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



