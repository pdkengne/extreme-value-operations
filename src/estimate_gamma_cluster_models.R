estimate_gamma_cluster_models <- function(x, cluster_data){
  # x:
  # cluster_data:
  
  if (length(cluster_data) == 1){
    cluster_models <- lapply(1:1, function(data){
      model <- fitdistrplus::fitdist(data = x, distr = "gamma", method = "mle")
      model
    })
  }
  else {
    cluster_models <- lapply(cluster_data, function(data){
      model <- fitdistrplus::fitdist(data = data, distr = "gamma", method = "mle")
      model
    })
  }
  
  cluster_models
}


# # example 1
# 
# source("./src/initialize_cluster_data.R")
# 
# n <- 1000
# x <- bmixture::rmixgamma(n = n, weight = c(1/2, 1/2), alpha = c(0.6, 1.3), beta = c(0.1, 0.1))
# 
# hist(x, nclass = 30)
# 
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_cluster_data(x = x, nclusters = nclusters)
# 
# initial_cluster_data
# 
# 
# cluster_models <- estimate_gamma_cluster_models(cluster_data = initial_cluster_data)
# 
# cluster_models
