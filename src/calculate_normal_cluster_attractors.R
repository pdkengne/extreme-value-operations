source("./src/get_knn.R")
source("./src/make_weights.R")


calculate_normal_cluster_attractors <- function(x, cluster_models, prior_cluster_weights){
  nclusters <- length(cluster_models)
  cluster_attractors_matrix <- sapply(1:nclusters, function(k){
    parameters <- cluster_models[[k]]
    densities <- dnorm(x = x, 
                       mean = parameters["mean"], 
                       sd = parameters["sd"])
    
    densities*prior_cluster_weights[k]
  })
  
  mass <- apply(cluster_attractors_matrix, 1, sum)
  cluster_attractors_matrix <- cluster_attractors_matrix/mass
  
  cluster_attractors_frequencies <- apply(cluster_attractors_matrix, 2, sum)
  
  cluster_attractors_weights <- make_weights(positives_values = cluster_attractors_frequencies)
  
  cluster_attractors_centers <- sapply(1:nclusters, function(k){
    sum(cluster_attractors_matrix[, k]*x)
  })
  
  data <- data.frame(x = x)
  
  cluster_data_list <- lapply(1:nclusters, function(k){
    center <- cluster_attractors_centers[k]
    size <- ceiling(cluster_attractors_frequencies[k])
    cluster_data_object <- get_knn(data = data, k = size, query = center)
    cluster_data <- x[cluster_data_object$id[1, ]]
    
    cluster_data
  })
  
  class(cluster_data_object$id[1, ])
  
  output <- list()
  
  output[["cluster_attractors_matrix"]] <- cluster_attractors_matrix
  output[["cluster_attractors_frequencies"]] <- cluster_attractors_frequencies
  output[["cluster_attractors_weights"]] <- cluster_attractors_weights
  output[["cluster_attractors_centers"]] <- cluster_attractors_centers
  output[["cluster_data_list"]] <- cluster_data_list
  
  output
}


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
# cluster_attractors$cluster_attractors_matrix
# 
# cluster_attractors$cluster_data_list



