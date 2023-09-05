source("./src/estimate_several_gev_models.R")
source("./src/estimate_gev_mixture_model_identic_weights.R")
source("./src/estimate_gev_mixture_model_pessimistic_weights.R")
source("./src/estimate_several_standardized_block_maxima_mean.R")
source("./src/estimate_gev_mixture_model_automatic_weights_mw.R")
source("./src/estimate_gev_mixture_model_automatic_weights_pw.R")

estimate_gev_mixture_model_parameters <- function(x, 
                                                  nsloc = NULL, 
                                                  std.err = FALSE, 
                                                  block_sizes = NULL, 
                                                  confidence_level = 0.95,
                                                  trace = TRUE){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # nsloc: dataframe of covariates for linear modeling of the location parameter
  # trace: boolean value which indicates whether to print information on the progress of optimization
  # std.err: a boolean which indicates whether the standard errors are returned or not
  # confidence_level: desired confidence level
  
  
  
  
  
  
  
  
}

















