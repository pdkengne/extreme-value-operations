source("./src/extract_block_maxima.R")
source("./src/extract_block_maxima_with_indexes.R")
source("./src/estimate_gev_parameters.R")
source("./src/calculate_power_gev_parameters.R")
source("./src/estimate_extremal_index.R")
source("./src/find_threshold_associated_with_given_block_size.R")


estimate_single_gev_model <- function(x, 
                                      block_size = 1,
                                      type = c("GEV", "Gumbel")[1],
                                      method = c("MLE", "GMLE", "Lmoments")[1]){
  # x: vector of observations
  # block_size: size of blocks to consider
  # type: type of model to use
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  # extract the vector of block maxima with indexes
  block_maxima_with_indexes <- extract_block_maxima_with_indexes(x, block_size)
  
  # extract block maxima
  block_maxima <- block_maxima_with_indexes$block_maxima
  
  # get the index of block maxima
  block_maxima_indexes <- block_maxima_with_indexes$block_maxima_indexes
  
  # calculate the extremal index
  threshold <- find_threshold_associated_with_given_block_size(x, block_size)
  extremal_index <- estimate_extremal_index(x = x, threshold = threshold)
  
  # estimate gev model
  gev_model <- estimate_gev_parameters(x = block_maxima,
                                       type = type,
                                       method = method)
  
  # calculate the normalized gev parameters with block size
  gev_parameters <- gev_model$results$par
  
  exponent_bs <- 1/block_size
  
  normalized_gev_parameters <- calculate_power_gev_parameters(loc = gev_parameters["location"], 
                                                              scale = gev_parameters["scale"], 
                                                              shape = gev_parameters["shape"], 
                                                              exponent = exponent_bs)
  
  # calculate the normalized gev parameters with both block size and extremal index
 exponent_bs_ei <- (1/block_size)*extremal_index
  
 full_normalized_gev_parameters <- calculate_power_gev_parameters(loc = gev_parameters["location"], 
                                                                  scale = gev_parameters["scale"], 
                                                                  shape = gev_parameters["shape"], 
                                                                  exponent = exponent_bs_ei)
  
  # update the output object
  output[["data"]] <- x
  output[["block_size"]] <- block_size
  output[["block_maxima"]] <- block_maxima
  output[["block_maxima_indexes"]] <- block_maxima_indexes
  output[["extremal_index"]] <- extremal_index
  output[["gev_model"]] <- gev_model
  output[["normalized_gev_parameters"]] <- normalized_gev_parameters
  output[["full_normalized_gev_parameters"]] <- full_normalized_gev_parameters
  
  output
}


# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)
# 
# block_size <- 25
# 
# results<- estimate_single_gev_model(x, block_size)
# 
# #results
# names(results)
# 
# # get the block size
# results$block_size
# 
# # get the extremal index
# results$extremal_index
# 
# # get block maxima
# results$block_maxima
# 
# # get block maxima indexes
# results$block_maxima_indexes
# 
# # get the normalized gev parameters
# results$normalized_gev_parameters
# results$full_normalized_gev_parameters
# 
# # get gev model
# model<- results$gev_model
# 
# model
# 
# names(model)
# 
# 
# # example 2
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# block_size <- 25
# 
# results<- estimate_single_gev_model(x, block_size)
# 
# #results
# names(results)
# 
# # get the block size
# results$block_size
# 
# # get the extremal index
# results$extremal_index
# 
# # get block maxima
# results$block_maxima
# 
# # get block maxima indexes
# results$block_maxima_indexes
# 
# # get the normalized gev parameters
# results$normalized_gev_parameters
# results$full_normalized_gev_parameters
# 
# # get gev model
# model<- results$gev_model
# 
# model
# 
# names(model)
# 
# 
# # example 3
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)
# 
# block_size <- 40
# 
# results<- estimate_single_gev_model(x, block_size)
# 
# #results
# names(results)
# 
# # get the block size
# results$block_size
# 
# # get the extremal index
# results$extremal_index
# 
# # get block maxima
# results$block_maxima
# 
# # get block maxima indexes
# results$block_maxima_indexes
# 
# # get the normalized gev parameters
# results$normalized_gev_parameters
# results$full_normalized_gev_parameters
# 
# # get gev model
# model<- results$gev_model
# 
# model
# 
# names(model)
