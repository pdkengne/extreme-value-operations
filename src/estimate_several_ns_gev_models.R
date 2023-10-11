source("./src/estimate_single_ns_gev_model.R")

estimate_several_ns_gev_models <- function(x, 
                                           block_sizes, 
                                           data = NULL, 
                                           location.fun = ~1,
                                           scale.fun = ~1, 
                                           shape.fun = ~1, 
                                           use.phi = TRUE,
                                           type = c("GEV", "Gumbel")[1],
                                           method = c("MLE", "GMLE")[1]){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # data: dataframe of covariates for linear modeling of the location parameter
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # type: type of model to use
  # method: estimation method to use
  
  # estimate the gev model associated with each block size
  models <- lapply(block_sizes, 
                   function(block_size)
                     estimate_single_ns_gev_model(x = x, 
                                                  data = data,
                                                  block_size = block_size, 
                                                  location.fun = location.fun,
                                                  scale.fun = scale.fun, 
                                                  shape.fun = shape.fun, 
                                                  use.phi = use.phi,
                                                  type = type,
                                                  method = method)) 
  
  names(models) <- block_sizes
  
  models
}


# # example 1
# 
# source("./src/find_minimum_block_size.R")
# source("./src/find_block_size_associated_with_given_number_of_blocks.R")
# source("./src/generate_gev_sample.R")
# source("./src/plot_several_ns_standardized_block_maxima_mean.R")
# source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
# 
# n <- 10000
# 
# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- (-49:50)/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# data <- data.frame(trend = trend, random = rnd)
# 
# minimum_block_size <- find_minimum_block_size(x)
# minimum_block_size
# 
# maximum_block_size <- find_block_size_associated_with_given_number_of_blocks(x, m = 50)
# maximum_block_size
# 
# block_sizes <- seq(from = minimum_block_size, to = maximum_block_size, by = 1)
# block_sizes
# 
# plot_several_ns_standardized_block_maxima_mean(x,
#                                                block_sizes,
#                                                confidence_level = 0.95,
#                                                equivalent = FALSE,
#                                                data = data,
#                                                location.fun = ~ .,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# 
# plot_several_ns_standardized_block_maxima_mean(x,
#                                                block_sizes,
#                                                confidence_level = 0.95,
#                                                equivalent = TRUE,
#                                                data = data,
#                                                location.fun = ~ .,
#                                                scale.fun = ~ .,
#                                                shape.fun = ~1,
#                                                use.phi = TRUE,
#                                                type = c("GEV", "Gumbel")[1],
#                                                method = c("MLE", "GMLE")[2])
# 
# equivalent_block_sizes_object<- estimate_several_ns_standardized_block_maxima_mean(x,
#                                                                                    block_sizes,
#                                                                                    confidence_level = 0.95,
#                                                                                    data = data,
#                                                                                    location.fun = ~ .,
#                                                                                    scale.fun = ~ .,
#                                                                                    shape.fun = ~1,
#                                                                                    use.phi = TRUE,
#                                                                                    type = c("GEV", "Gumbel")[1],
#                                                                                    method = c("MLE", "GMLE")[2])
# equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
# 
# results <- estimate_several_ns_gev_models(x = x,
#                                           block_sizes = equivalent_block_sizes,
#                                           data = data,
#                                           location.fun = ~ .,
#                                           scale.fun = ~ .,
#                                           shape.fun = ~ 1,
#                                           use.phi = TRUE,
#                                           type = c("GEV", "Gumbel")[1],
#                                           method = c("MLE", "GMLE")[2])
# 
# #results
# names(results)
# 
# # extract a model
# model <- results[[1]]
# 
# names(model)
# 
# # model
