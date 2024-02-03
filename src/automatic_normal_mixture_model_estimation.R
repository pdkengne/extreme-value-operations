# library(bmixture)
# library(factoextra)
# library(zoo)
# library(FactoMineR)


library(fitdistrplus)
library(dplyr)
library(dbscan)
library(bmixture)


source("./src/get_knn.R")
source("./src/initialize_cluster_data.R")
source("./src/calculate_normal_mixture_model_cdf.R")
source("./src/calculate_normal_mixture_model_pdf.R")
source("./src/calculate_normal_mixture_model_inverse_cdf.R")



n <- 100
x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(-10, +10), sd = c(1, 1))


nclusters <- 3

initial_cluster_data <- initialize_cluster_data(x = x, nclusters = 3)

initial_cluster_data



estimate_cluster_models <- function(cluster_data){
  cluster_models <- lapply(cluster_data, function(data){
    model <- fitdistrplus::fitdist(data = data, distr = "norm", method = "mle")
    model$estimate
  })
  
  cluster_models
}


cluster_models <- estimate_cluster_models(cluster_data = initial_cluster_data)

cluster_models



support <- seq(from = min(x), to = max(x), by = 1)
hist(x, probability = TRUE)
lines(density(x))


lapply(1:length(cluster_models), function(k){
  parameters <- cluster_models[[k]]
  densities <- dnorm(x = support, 
                     mean = parameters["mean"], 
                     sd = parameters["sd"])
  lines(x = support, y = densities, col = k + 1)
})


make_weights <- function(positives_values){
  weights <- positives_values/sum(positives_values)
}


get_knn <- function(data, k, query = NULL, search = "kdtree"){
  # data: a data matrix
  # k: the maximum number of nearest neighbors to search
  # query: a data matrix with the points to query
  # search: nearest neighbor search strategy (one of "kdtree", "linear" or "dist")
  
  output <- dbscan::kNN(x = data,
                        k = k, 
                        query = query, 
                        search = search,
                        sort = TRUE)
  
  output
}




calculate_cluster_attractors <- function(x, cluster_models, prior_cluster_weights){
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



prior_cluster_weights <- make_weights(positives_values = rep(1, times = n))

prior_cluster_weights


cluster_attractors <- calculate_cluster_attractors(x = x, 
                                                   cluster_models = cluster_models, 
                                                   prior_cluster_weights = prior_cluster_weights)

names(cluster_attractors)

cluster_attractors$cluster_attractors_centers

cluster_attractors$cluster_attractors_frequencies

cluster_attractors$cluster_attractors_weights

cluster_attractors$cluster_attractors_matrix

cluster_attractors$cluster_data_list


# loop

cluster_models <- estimate_cluster_models(cluster_data = cluster_attractors$cluster_data_list)

cluster_models

support <- seq(from = min(x), to = max(x), by = 1)
hist(x, probability = TRUE)
lines(density(x))

lapply(1:length(cluster_models), function(k){
  parameters <- cluster_models[[k]]
  densities <- dnorm(x = support, 
                     mean = parameters["mean"], 
                     sd = parameters["sd"])
  lines(x = support, y = densities, col = k + 1)
})


cluster_attractors <- calculate_cluster_attractors(x = x, 
                                                   cluster_models = cluster_models, 
                                                   prior_cluster_weights = cluster_attractors$cluster_attractors_weights)

names(cluster_attractors)

cluster_attractors$cluster_attractors_centers

cluster_attractors$cluster_attractors_frequencies

cluster_attractors$cluster_attractors_weights

cluster_attractors$cluster_attractors_matrix

cluster_attractors$cluster_data_list





















