initialize_cluster_data <- function(x, nclusters = 3){
  # x:
  # nclusters:
  
  n <- length(x)
  
  cluster_data <- lapply(1:nclusters, function(k){
    set.seed(k)
    posistions <- sort(unique(sample.int(n = n, replace = TRUE)))
    x[posistions]
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
