# library(ggplot2)

source("./src/calculate_modes.R")
source("./src/make_weights.R")


extract_cluster_infos <- function(x, nclusters = NULL){
  # x:
  # nclusters:
  
  n <- length(x)
  
  Fn <- ecdf(x = x)
  
  empirical_cdf_object <- EnvStats::ecdfPlot(x = x, plot.pos.con = 0.375, plot.it = FALSE)  
  
  empirical_cdf <- empirical_cdf_object$Cumulative.Probabilities
  
  exponential_sample <- qexp(p = empirical_cdf, rate = 1)
  
  if (is.null(nclusters)){
    modes_object <- calculate_modes(x = x)
    modes <- modes_object$density_maxima_argument
    kmeans_object <- kmeans(x = exponential_sample, centers = length(modes))
  }
  else {
    kmeans_object <- kmeans(x = exponential_sample, centers = nclusters)
  }
  
  centers <- kmeans_object$centers[, 1]
  
  sizes <- kmeans_object$size
  
  clusters <- kmeans_object$cluster
  
  cluster_weights <- make_weights(positives_values = sizes)

  cluster_data <- lapply(1:nclusters, function(k){
    positions <- which(clusters == k)
    
    data <- x[positions]
    
    data
  })
  
  output <- list()
  
  output[["cluster_data"]] <- cluster_data
  output[["cluster_weights"]] <- cluster_weights
  
  output
}


# # example 1
# 
# n <- 1000
# x <- rnorm(n = n)
# 
# 
# nclusters <- 3
# 
# cluster_infos <- extract_cluster_infos(x = x, nclusters = nclusters)
# 
# cluster_infos
