# library(ggplot2)

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
  
  sizes <- kmeans_object$size
  
  if (length(sizes) == 1){
    sizes <- n - 1
  }
  
  nclusters <- length(centers)
  
  data <- data.frame(x = x)

  cluster_data <- lapply(1:nclusters, function(k){
    center <- centers[k]
    
    size <- sizes[k]
    
    cluster_data_object <- get_knn(data = data, k = size, query = center)
    cluster_data <- x[cluster_data_object$id[1, ]]
    
    cluster_data
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
# initial_cluster_data <- initialize_cluster_data(x = x, nclusters = 3)
# 
# initial_cluster_data
