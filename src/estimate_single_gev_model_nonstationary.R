source("./src/extract_block_maxima.R")
source("./src/extract_block_maxima_with_indexes.R")
source("./src/estimate_gev_parameters.R")
source("./src/calculate_power_gev_parameters.R")
source("./src/estimate_extremal_index.R")
source("./src/find_threshold_associated_with_given_block_size.R")
source("./src/estimate_gev_model_parameters.R")


estimate_single_gev_model_nonstationary_nonstationary <- function(x, 
                                                                  data = NULL, 
                                                                  block_size = 1,
                                                                  threshold = NULL, 
                                                                  threshold.fun = ~1, 
                                                                  location.fun = ~1,
                                                                  scale.fun = ~1, 
                                                                  shape.fun = ~1, 
                                                                  use.phi = FALSE,
                                                                  type = c("GEV", "GP", "PP", "Gumbel", "Exponential")[1],
                                                                  method = c("MLE", "GMLE", "Bayesian", "Lmoments")[1]){
  # x: vector of observations
  # block_size: size of blocks to consider
  # data: dataframe of covariates for linear modeling of the location parameter
  # threshold.fun, location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                                    must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter or not
  # type: type of model to use
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  # add x to the data
  data <- data.frame(data, "x" = x)
  
  # extract the vector of block maxima with indexes
  block_maxima_with_indexes <- extract_block_maxima_with_indexes(x, block_size)
  
  # extract block maxima
  block_maxima <- block_maxima_with_indexes$block_maxima
  
  # get the index of block maxima
  block_maxima_indexes <- block_maxima_with_indexes$block_maxima_indexes
  
  # calculate the extremal index
  threshold <- find_threshold_associated_with_given_block_size(x, block_size)
  extremal_index <- estimate_extremal_index(x, threshold, run = 0)
  
  # estimate gev model
  gev_model <- estimate_gev_model_parameters(x = block_maxima, 
                                             data = data,
                                             threshold = threshold, 
                                             threshold.fun = threshold.fun, 
                                             location.fun = location.fun,
                                             scale.fun = scale.fun, 
                                             shape.fun = shape.fun, 
                                             use.phi = use.phi,
                                             type = type,
                                             method = method)
    
  # calculate the normalized gev parameters
  gev_parameters <- fit_extRemes_z$results$par
  normalized_gev_parameters <- calculate_power_gev_parameters(loc = gev_parameters["location"], 
                                                              scale = gev_parameters["scale"], 
                                                              shape = gev_parameters["shape"], 
                                                              exponent = 1/block_size)
  
  # update the output object
  output[["data"]] <- x
  output[["covariates"]] <- data
  output[["block_size"]] <- block_size
  output[["block_maxima"]] <- block_maxima
  output[["block_maxima_indexes"]] <- block_maxima_indexes
  output[["gev_parameters"]] <- gev_parameters
  output[["normalized_gev_parameters"]] <- normalized_gev_parameters
  output[["extremal_index"]] <- extremal_index
  output[["gev_model"]] <- gev_model
  
  output
}



# example 1

source("./src/generate_gev_sample.R")

x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = +0.2)

block_size <- 25

results<- estimate_single_gev_model_nonstationary(x, data = NULL, block_size = block_size,)

#results
names(results)

# get the block size
results$block_size

# get the extremal index
results$extremal_index

# get the normalized gev parameters
results$normalized_gev_parameters

# get gev model
model<- results$gev_model

model

names(model)

# get block maxima
model$data

# get block maxima indexes
results$block_maxima_indexes


# example 2

source("./src/generate_gev_sample.R")

x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)

block_size <- 25

results<- estimate_single_gev_model_nonstationary(x, block_size, nsloc = NULL)

#results
names(results)

# get the block size
results$block_size

# get the extremal index
results$extremal_index

# get the normalized gev parameters
results$normalized_gev_parameters

# get gev model
model<- results$gev_model

model

names(model)

# get block maxima
model$data

# get block maxima indexes
results$block_maxima_indexes


# example 3

source("./src/generate_gev_sample.R")

x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = 0)

block_size <- 40

results<- estimate_single_gev_model_nonstationary(x, block_size, nsloc = NULL)

#results
names(results)

# get the block size
results$block_size

# get the extremal index
results$extremal_index

# get the normalized gev parameters
results$normalized_gev_parameters

# get gev model
model<- results$gev_model

model

names(model)

# get block maxima
model$data

# get block maxima indexes
results$block_maxima_indexes
