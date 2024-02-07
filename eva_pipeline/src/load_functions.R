# Options pour les différentes étapes
options(tidyverse.quiet = TRUE)
options(caret.quiet = TRUE)
options(Hmisc.quiet = TRUE)
options(vroom.quiet = TRUE)


# load functions
load_functions <- function(){
  
  # load external libraries
  library(tidyverse)
  library(readr)
  library(DescTools)
  library(Hmisc)
  library(vroom)
  
  
  # load internal functions
  source("./src/impute_outliers.R")
  source("./src/extract_nlargest_sample.R")
  
  source("./src/extract_block_maxima.R")
  source("./src/extract_block_maxima_with_indexes.R")
  
  source("./src/get_standard_scaled_data.R")
  source("./src/get_one_hot_encoded_data.R")
  source("./src/get_one_hot_encoded_and_standard_scaled_data.R")
  
  source("./src/calculate_modes.R")
  source("./src/plot_modes.R")
  
  source("./src/calculate_gev_cdf.R")
  source("./src/calculate_gev_pdf.R")
  source("./src/calculate_gev_inverse_cdf.R")
  source("./src/generate_gev_sample.R")
  
  source("./src/estimate_gev_parameters.R")
  source("./src/estimate_gev_model_quantile.R")
  source("./src/estimate_single_gev_model.R")
  source("./src/estimate_several_gev_models.R")
  
  source("./src/estimate_ns_gev_parameters.R")
  source("./src/estimate_single_ns_gev_model.R")
  source("./src/estimate_several_ns_gev_models.R")
  
  source("./src/get_ns_gev_model_normalized_parameters.R")
  source("./src/get_several_ns_gev_model_normalized_parameters.R")
  
  source("./src/calculate_gev_mixture_model_pdf.R")
  source("./src/calculate_gev_mixture_model_cdf.R")
  source("./src/calculate_gev_mixture_model_inverse_cdf.R")
  source("./src/generate_gev_mixture_model_sample.R")
  
  source("./src/estimate_several_standardized_block_maxima_mean.R")
  source("./src/estimate_several_ns_standardized_block_maxima_mean.R")
  
  source("./src/plot_several_standardized_block_maxima_mean.R")
  source("./src/plot_several_ns_standardized_block_maxima_mean.R")
  
  source("./src/fit_stationary_gev_mixture_model.R")
  source("./src/fit_non_stationary_gev_mixture_model.R")
  
  source("./src/plot_fit_stationary_gev_mixture_model.R")
  source("./src/plot_fit_non_stationary_gev_mixture_model.R")
  
  source("./src/calculate_stationary_gev_mixture_model_cdf.R")
  source("./src/calculate_stationary_gev_mixture_model_inverse_cdf.R")
  source("./src/calculate_stationary_gev_mixture_model_pdf.R")
  source("./src/generate_stationary_gev_mixture_model_sample.R")
  
  source("./src/calculate_non_stationary_gev_mixture_model_cdf.R")
  source("./src/calculate_non_stationary_gev_mixture_model_inverse_cdf.R")
  source("./src/calculate_non_stationary_gev_mixture_model_pdf.R")
  source("./src/generate_non_stationary_gev_mixture_model_sample.R")
  
  source("./src/estimate_stationary_gev_mixture_model_quantile.R")
  source("./src/estimate_non_stationary_gev_mixture_model_quantile.R")
  
  source("./src/plot_estimate_stationary_gev_mixture_model_quantile.R")
  source("./src/plot_estimate_non_stationary_gev_mixture_model_quantile.R")
  
  source("./src/fit_stationary_normal_mixture_model.R")
  source("./src/plot_fit_stationary_normal_mixture_model.R")
  
  source("./src/fit_stationary_weibull_mixture_model.R")
  source("./src/plot_fit_stationary_weibull_mixture_model.R")
  
  source("./src/fit_stationary_gamma_mixture_model.R")
  source("./src/plot_fit_stationary_gamma_mixture_model.R")
  
  source("./src/fit_stationary_lnorm_mixture_model.R")
  source("./src/plot_fit_stationary_lnorm_mixture_model.R")
  
  source("./src/fit_stationary_evd_mixture_model.R")
  source("./src/plot_fit_stationary_evd_mixture_model.R")
  
  source("./src/fit_stationary_lgamma_mixture_model.R")
  source("./src/plot_fit_stationary_lgamma_mixture_model.R")
  
}

