make_weights <- function(positives_values){
  # positives_values:
  
  weights <- positives_values/sum(positives_values)
}


# # example 1
# 
# n <- 100
# 
# prior_cluster_weights <- make_weights(positives_values = rep(1, times = n))
# 
# prior_cluster_weights
