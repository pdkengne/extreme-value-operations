# library(ggplot2)
library(dplyr)

source("./src/calculate_modes.R")

initialize_ns_cluster_data <- function(x,
                                       covariates = NULL,
                                       nclusters = NULL, 
                                       centers = NULL){
  # x:
  # nclusters:
  # covariates:
  # centers:
  
  nstart <- 25
  
  n <- length(x)
  
  if (is.null(centers) & is.null(nclusters)){
    modes_object <- calculate_modes(x = x)
    modes <- modes_object$density_maxima_argument
    kmeans_object <- kmeans(x = x, centers = length(modes), nstart = nstart)
  }
  else if (is.null(centers) & !is.null(nclusters)){
    kmeans_object <- kmeans(x = x, centers = nclusters, nstart = nstart)
  }
  else if (!is.null(centers) & is.null(nclusters)){
    kmeans_object <- kmeans(x = x, centers = nclusters, nstart = nstart)
  }
  else if (!is.null(centers) & !is.null(nclusters)){
    kmeans_object <- kmeans(x = x, centers = nclusters, nstart = nstart)
  }
  
  centers <- kmeans_object$centers[, 1]
  
  clusters <- kmeans_object$cluster
  
  nclusters <- length(centers)
  
  cluster_data <- lapply(1:nclusters, function(k){
    positions <- which(clusters == k)
    
    data <- x[positions]
    
    data
  })
  
  
  if (is.null(covariates)){
    covariates <- data.frame("intercept" = rep(x = 1, times = length(x)))
  }
  
  
  cluster_covariates <- lapply(1:nclusters, function(k){
    positions <- which(clusters == k)
    
    predictors <- covariates %>% slice(positions)
    
    predictors
  })
  
  
  output <- list()
  
  output[["cluster_data"]] <- cluster_data
  output[["cluster_covariates"]] <- cluster_covariates
  
  output
}


# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# n <- 100
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(-10, +10), sd = c(1, 1))
# 
# trend <- 1:n/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# covariates <- data.frame(trend = trend, random = rnd)
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_ns_cluster_data(x = x,
#                                                    covariates = covariates,
#                                                    nclusters = nclusters)
# 
# names(initial_cluster_data)
# 
# initial_cluster_data$cluster_data
# 
# initial_cluster_data$cluster_covariates
