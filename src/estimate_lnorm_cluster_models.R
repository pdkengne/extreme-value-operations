estimate_lnorm_cluster_models <- function(x, cluster_data){
  # x:
  # cluster_data:
  
  if (length(cluster_data) == 1){
    cluster_models <- lapply(1:1, function(data){
      model <- fitdistrplus::fitdist(data = x, distr = "lnorm", method = "mle")
      model
    })
  }
  else {
    cluster_models <- lapply(cluster_data, function(data){
      model <- fitdistrplus::fitdist(data = data, distr = "lnorm", method = "mle")
      model
    })
  }

  cluster_models
}


# # example 1
# 
# source("./src/initialize_cluster_data.R")
# source("./src/generate_lnorm_mixture_model_sample.R")
# 
# n <- 1000
# x <- generate_lnorm_mixture_model_sample(n = n, 
#                                          locations = c(0, 1), 
#                                          scales = c(0.5, 0.25), 
#                                          weights = c(1/2, 1/2), 
#                                          kind = c("geometric", "arithmetic")[2])
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
# cluster_models <- estimate_lnorm_cluster_models(cluster_data = initial_cluster_data)
# 
# cluster_models
