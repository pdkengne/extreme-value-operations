# library(ggplot2)

source("./src/get_knn.R")
source("./src/calculate_modes.R")

initialize_cluster_data <- function(x, nclusters = NULL, centers = NULL){
  # x:
  # nclusters:
  # centers:
  
  n <- length(x)
  
  if (is.null(centers) & is.null(nclusters)){
    modes_object <- calculate_modes(x = x)
    modes <- modes_object$density_maxima_argument
    kmeans_object <- kmeans(x = x, centers = length(modes))
  }
  else if (is.null(centers) & !is.null(nclusters)){
    kmeans_object <- kmeans(x = x, centers = nclusters)
  }
  else if (!is.null(centers) & is.null(nclusters)){
    kmeans_object <- kmeans(x = x, centers = centers)
  }
  else if (!is.null(centers) & !is.null(nclusters)){
    kmeans_object <- kmeans(x = x, centers = nclusters)
  }
  
  centers <- kmeans_object$centers[, 1]
  
  clusters <- kmeans_object$cluster
  
  nclusters <- length(centers)
  
  cluster_data <- lapply(1:nclusters, function(k){
    positions <- which(clusters == k)
    
    data <- x[positions]
    
    data
  })
  
  cluster_data
}


# # example 1
# 
# n <- 100
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(-10, +10), sd = c(1, 1))
# 
# 
# nclusters <- 3
# 
# initial_cluster_data <- initialize_cluster_data(x = x, nclusters = nclusters)
# 
# initial_cluster_data
