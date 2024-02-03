source("./src/initialize_cluster_data.R")

estimate_normal_cluster_models <- function(cluster_data){
  # cluster_data:
  
  cluster_models <- lapply(cluster_data, function(data){
    model <- fitdistrplus::fitdist(data = data, distr = "norm", method = "mle")
    model$estimate
  })
  
  cluster_models
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
# 
# 
# cluster_models <- estimate_normal_cluster_models(cluster_data = initial_cluster_data)
# 
# cluster_models
