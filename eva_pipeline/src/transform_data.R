source("./eva_pipeline/src/load_functions.R")

load_functions()

library(dplyr)
library(tidyr)

# load created function
transform_data <- function(data, 
                           response_var,
                           response_abs = FALSE,
                           scale_predictors = TRUE,
                           coefficient_iqr = Inf, 
                           iterate = 1,
                           remove_outliers = FALSE,
                           method = c("interpolate", "mode", "median", "mean")[1]){
  # data:
  # response_var:
  # response_abs:
  # scale_predictors:
  # coefficient_iqr:
  # iterate:
  # remove_outliers:
  # method:
  
  response <- data %>% select(all_of(response_var))
  
  if (response_abs){
    response <- abs(response[, 1])
  }
  else{
    response <- response[, 1]
  }
  
  response_var_object <- impute_outliers(x = response, 
                                         coefficient_iqr = coefficient_iqr, 
                                         iterate = iterate,
                                         method = method)
  
  outlier_positions <- response_var_object$outlier_positions
  outlier_values <- response_var_object$outlier_values
  outlier_substitutes <- response_var_object$outlier_substitutes
  iterate <- response_var_object$iterate
  
  outlier_values
  
  if (remove_outliers & length(outlier_positions) != 0){
    x <- response[-outlier_positions]
    data_clean <- data[-outlier_positions, ]
  }
  else{
    x <- response_var_object$imputed_data
    data_clean <- data
  }
  
  data_covariates_unscaled <- data_clean %>% select(!c(file, timestamp, latitude, longitude, 
                                                       lateral_error, longitudinal_error, latitude_error,
                                                       longitude_error, velocity_latitude, velocity_longitude))
  
  if (scale_predictors){
    data_covariates_scaled <- data_covariates_unscaled[, !apply(is.na(data_covariates_unscaled), 2, all)]
    # data_covariates_scaled <- na.omit(data_covariates_scaled)
    # data_covariates_scaled <- data_covariates_scaled %>% mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
    # data_covariates_scaled <- data_covariates_scaled %>% mutate_at(all_of(names(data_covariates_scaled)), 
    #                                                                ~replace_na(., mean(., na.rm = TRUE)))
    data_covariates_scaled <- data_covariates_scaled %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
    data_covariates_scaled <- get_standard_scaled_data(data = data_covariates_scaled, newdata = NULL)
  }
  
  output <- list(response = x,
                 predictors = data_covariates_scaled,
                 unscaled_predictors = data_covariates_unscaled,
                 outlier_positions = outlier_positions,
                 outlier_values = outlier_values,
                 outlier_substitutes = outlier_substitutes,
                 iterate = iterate)
  
}


