source("./src/extract_block_maxima.R")
source("./src/estimate_gev_parameters.R")

# https://www.econometrics-with-r.org/3.4-confidence-intervals-for-the-population-mean.html

estimate_standardized_block_maxima_mean <- function(x, block_size = 1, confidence_interval_level = 0.95){
  # x: vector of observations
  # block_size: size of blocks to consider
  # confidence_interval_level: desired confidence interval level
  
  # extract block maxima
  block_maxima <- extract_block_maxima(x, block_size)
  
  # estimate gev model
  gev_model <- estimate_gev_parameters(x = block_maxima, nsloc = NULL)
  
  # extract gev model parameters
  gev_model_parameters <- gev_model$estimate
  
  # calculate standardized block maxima
  standardized_block_maxima <- (block_maxima - gev_model_parameters["loc"])/gev_model_parameters["scale"]
  
  # calculate the confidence interval of the standardized block maxima mean
  estimated_mean_confidence_interval <- t.test()


    estimated_mean_confidence_interval
}

# https://www.econometrics-with-r.org/3.4-confidence-intervals-for-the-population-mean.html












