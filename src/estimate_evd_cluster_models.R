source("./src/estimate_gev_parameters.R")


estimate_evd_cluster_models <- function(x, cluster_data){
  # x:
  # cluster_data:
  
  if (length(cluster_data) == 1){
    cluster_models <- lapply(1:1, function(data){
      model <- estimate_gev_parameters(x = x, method = "MLE")
      model
    })
  }
  else {
    cluster_models <- lapply(cluster_data, function(data){
      model <- estimate_gev_parameters(x = data, method = "MLE")
      model
    })
  }
  
  cluster_models
}


# example 1

source("./src/initialize_cluster_data.R")

n <- 1000
x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(0.6, 1.3), sd = c(0.1, 0.1))

hist(x, nclass = NULL)


nclusters <- 2

initial_cluster_data <- initialize_cluster_data(x = x, nclusters = nclusters)

initial_cluster_data


cluster_models <- estimate_evd_cluster_models(cluster_data = initial_cluster_data)

cluster_models


# example 2

source("./src/initialize_cluster_data.R")
source("./src/generate_gev_mixture_model_sample.R")

n <- 1000
x <- generate_gev_mixture_model_sample(n = n, 
                                       weights = c(1/2, 1/2), 
                                       locations = c(-3, +3), 
                                       scales = c(1, 1), 
                                       shapes = c(-0.01, +0.01),
                                       kind = c("geometric", "arithmetic", "harmonic")[2])

hist(x, nclass = 30)


nclusters <- 2

initial_cluster_data <- initialize_cluster_data(x = x, nclusters = nclusters)

initial_cluster_data


cluster_models <- estimate_evd_cluster_models(cluster_data = initial_cluster_data)

cluster_models
