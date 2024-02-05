# library(ggplot2)

source("./src/calculate_modes.R")

initialize_cluster_data <- function(x, nclusters = NULL, centers = NULL, sizes = NULL){
  # x:
  # nclusters:
  # centers:
  # sizes:
  
  n <- length(x)
  
  if (is.null(centers) & is.null(nclusters)){
    modes_object <- calculate_modes(x = x)
    centers <- modes_object$density_maxima_argument
  }
  else if (is.null(centers) & !is.null(nclusters)){
    y <- sort(x)
    split_indicators <- as.numeric(ggplot2::cut_number(x = 1:n, n = nclusters))
    centers <- sapply(1:nclusters, function(k){
      mean(y[which(split_indicators == k)])
    })
  }
  
  nclusters <- length(centers)
  
  if (is.null(sizes)){
    if (nclusters == 1){
      sizes <- n - 1
    }
    else {
      sizes <- rep(x = ceiling(n/nclusters), times = nclusters)
    }
  }
  else if (length(centers) != length(sizes)){
    stop("Sorry, the arguments 'centers' and 'sizes' must have the same length!")
  }
  
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
