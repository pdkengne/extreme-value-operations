# load external libraries
library(tidyverse)
library(readr)
library(DescTools)
library(Hmisc)


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



# load created function
make_models <- function(variable = ~1){
  # variable:
  
  if (variable == ~1){
    models_object <- c(list("model_00" = list("location.fun" = variable, "scale.fun" = variable, "shape.fun" = variable)))
  }
  else{
    models_object <- c(list("model_01" = list("location.fun" = variable, "scale.fun" = variable, "shape.fun" = variable),
                            "model_02" = list("location.fun" = variable, "scale.fun" = ~1, "shape.fun" = ~1),
                            "model_03" = list("location.fun" = ~1, "scale.fun" = variable, "shape.fun" = ~1),
                            "model_04" = list("location.fun" = ~1, "scale.fun" = ~1, "shape.fun" = variable),
                            "model_05" = list("location.fun" = variable, "scale.fun" = variable, "shape.fun" = ~1),
                            "model_06" = list("location.fun" = variable, "scale.fun" = ~1, "shape.fun" = variable),
                            "model_07" = list("location.fun" = ~1, "scale.fun" = variable, "shape.fun" = variable)))
  }
  
  models_object
  
}




# load created function
transform_data <- function(data_path, 
                           response_var,
                           scale_predictors = TRUE,
                           coefficient_iqr = Inf, 
                           remove_outliers = FALSE,
                           method = c("interpolate", "mode", "median", "mean")[1]){
  # data_path:
  # response_var:
  # scale_predictors:
  # coefficient_iqr:
  # remove_outliers:
  # method:
  
  data <- read.csv(data_path)
  
  response <- data %>% select(all_of(response_var))
  
  response <- response[, 1]
  
  response_var_object <- impute_outliers(x = response, coefficient_iqr = coefficient_iqr, method = method)
  
  outlier_positions <- response_var_object$outlier_positions
  
  if (remove_outliers & length(outlier_positions) != 0){
    x <- response[-outlier_positions]
    data_clean <- data[-outlier_positions, ]
  }
  else{
    x <- response_var_object$imputed_data
    data_clean <- data
  }

  detection_vars <- names(data_clean)[c(11:45)]
  predictor_vars <- c("velocity", "object", "area", "horizontal_left", "horizontal_right", 
                      "vertical_down", "vertical_up", detection_vars)
  
  data_covariates <- data_clean %>% select(all_of(predictor_vars))
  
  data_covariates_clean <- data_covariates %>% select_if(colSums(.) != 0)
  
  if (scale_predictors){
    data_covariates_clean <- get_standard_scaled_data(data_covariates_clean, newdata = NULL)
  }
  
  output <- list(response = x,
                 predictors = data_covariates_clean)
  
}



# load created function
calculate_model_infos <- function(x, data, models){
  # x:
  # data:
  # models:
  
  use.phi <- TRUE
  nlargest <- 10000
  block_sizes <- NULL
  minimum_nblocks <- 50
  threshold <- min(x)
  confidence_level <- 0.95
  use_extremal_index <- TRUE
  use_uniform_prior <- TRUE
  method <- "MLE"
  
  fitted_models_object <- lapply(models, function(model){
    try({
      ns_gev_mixture_model_object <- fit_non_stationary_gev_mixture_model(x = x,
                                                                          data = data,
                                                                          location.fun = model$location.fun,
                                                                          scale.fun = model$scale.fun,
                                                                          shape.fun = model$shape.fun,
                                                                          use.phi = use.phi,
                                                                          nlargest = nlargest,
                                                                          block_sizes = block_sizes,
                                                                          minimum_nblocks = minimum_nblocks,
                                                                          threshold = threshold,
                                                                          confidence_level = confidence_level,
                                                                          use_extremal_index = use_extremal_index,
                                                                          use_uniform_prior = use_uniform_prior,
                                                                          method = method)
      
      model_vector <- as.character(model)
      names(model_vector) <- c("location.fun", "scale.fun", "shape.fun")
      
      information_criteria <- ns_gev_mixture_model_object$information_criteria
      
      c(model_vector, information_criteria)
    },
    silent = TRUE)
    
  })
  
  success <- sapply(fitted_models_object, function(x) !inherits(x, "try-error"))
  
  fitted_models_object_success <- fitted_models_object[success]
  
  fitted_models_object_success_df <- do.call(rbind, fitted_models_object_success)
  
  fitted_models_object_success_df <- data.frame("model_names" = rownames(fitted_models_object_success_df),
                                                fitted_models_object_success_df)
  
  rownames(fitted_models_object_success_df) <- 1:nrow(fitted_models_object_success_df)
  
  fitted_models_object_success_df
  
}




# load created function
save_model_infos <- function(main_dir, 
                             response_var,
                             variable = ~1,
                             scale_predictors = TRUE,
                             coefficient_iqr = Inf, 
                             remove_outliers = FALSE,
                             method = c("interpolate", "mode", "median", "mean")[1]){
  # main_dir:
  # response_var:
  # variable:
  # scale_predictors:
  # coefficient_iqr:
  # remove_outliers:
  # method:
  
  folder_name <- as.character(variable)[-1]
  
  output_csv_folder <- file.path(main_dir, folder_name)
  
  if (!dir.exists(output_csv_folder)){
    dir.create(output_csv_folder)
  }
  
  output_csv_file <- file.path(output_csv_folder, "fitted_models_information.csv")
  
  input_csv_file <- file.path(main_dir, "merged_data.csv")
  
  data_object <- transform_data(data_path = input_csv_file, 
                                response_var = response_var,
                                scale_predictors = scale_predictors,
                                coefficient_iqr = coefficient_iqr, 
                                remove_outliers = remove_outliers,
                                method = method)
  
  x <- data_object$response
  
  data <- data_object$predictors
  
  models <- make_models(variable)
  
  model_infos <- calculate_model_infos(x = x, data = data, models = models)
  
  write.csv(x = model_infos, file = output_csv_file, row.names = FALSE)
  
}




# use created function
setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

defaultW <- getOption("warn") 
options(warn = -1) 


main_dir  <- "./04_experimentation_01"

response_var <- "lateral_error"

save_model_infos(main_dir = main_dir,
                 response_var = response_var,
                 variable = ~object, 
                 scale_predictors = TRUE,
                 coefficient_iqr = 9, 
                 remove_outliers = FALSE,
                 method = c("interpolate", "mode", "median", "mean")[1])


options(warn = defaultW)




















