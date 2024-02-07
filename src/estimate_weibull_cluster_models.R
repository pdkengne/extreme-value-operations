estimate_weibull_cluster_models <- function(cluster_data){
  # x:
  # cluster_data:
  
  cluster_models <- lapply(cluster_data, function(data){
    model <- fitdistrplus::fitdist(data = data, distr = "weibull", method = "mle")
    model
  })
  
  cluster_models
}


# # example 1
# 
# source("./src/initialize_cluster_data.R")
# 
# n <- 1000
# x <- mixR::rmixweibull(n = n, pi = c(1/2, 1/2), mu = c(0.6, 1.3), sd = c(0.1, 0.1))
# 
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_cluster_data(x, nclusters = nclusters)
# 
# initial_cluster_data
# 
# 
# cluster_models <- estimate_weibull_cluster_models(cluster_data = initial_cluster_data)
# 
# cluster_models
