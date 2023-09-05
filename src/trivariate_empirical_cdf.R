# empirical distribution function for a trivariate data

trivariate_empirical_cdf <- function(x, X){
  # x: vector with 3 numerical values
  # X: dataframe with 3 columns of numerical values
  
  trivariate_cdf <- mean(X[, 1] <= x[1] & X[, 2] <= x[2] & X[, 3] <= x[3])
  
  trivariate_cdf
}


# # example 1
# 
# n <- 1000
# 
# X <- cbind(rnorm(n = n), rnorm(n = n), rnorm(n = n))
# 
# x <- c(rnorm(n = 1), rnorm(n = 1), rnorm(n = 1))
# 
# results <- trivariate_empirical_cdf(x, X)
# 
# results
# 
# 
# # example 2
# 
# source("./src/estimate_several_gev_models.R")
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/estimate_several_standardized_block_maxima_mean.R")
# 
# x <- rnorm(n = 10000)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# 
# equivalent_block_sizes_object<- estimate_several_standardized_block_maxima_mean(x, block_sizes, confidence_level = 0.95)
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# gev_models <- estimate_several_gev_models(x, block_sizes = equivalent_block_sizes, nsloc = NULL)
# 
# X <- gev_models$normalized_gev_parameters
# 
# results <- apply(X, 1, FUN = trivariate_empirical_cdf, X)
# 
# results
# 
# unique(results)


