# library(ggplot2)

source("./src/calculate_modes.R")

initialize_cluster_data <- function(x, nclusters = NULL, centers = NULL){
  # x:
  # nclusters:
  # centers:
  
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
  
  data <- data.frame(x = x)

  cluster_data <- lapply(1:nclusters, function(k){
    center <- centers[k]
    
    size <- ifelse(test = nclusters == 1,
                   yes = ceiling(n - 1),
                   no = ceiling(n/nclusters))
    
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
