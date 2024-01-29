source("./eva_pipeline/src/load_functions.R")

load_functions()

# load created function
transform_data <- function(data, 
                           response_var,
                           response_abs = FALSE,
                           scale_predictors = TRUE,
                           coefficient_iqr = Inf, 
                           remove_outliers = FALSE,
                           method = c("interpolate", "mode", "median", "mean")[1]){
  # data:
  # response_var:
  # response_abs:
  # scale_predictors:
  # coefficient_iqr:
  # remove_outliers:
  # method:
  
  response <- data %>% select(all_of(response_var))
  
  if (response_abs){
    response <- abs(response[, 1])
  }
  else{
    response <- response[, 1]
  }
  
  response_var_object <- impute_outliers(x = response, coefficient_iqr = coefficient_iqr, method = method)
  
  outlier_positions <- response_var_object$outlier_positions
  outlier_values <- response_var_object$outlier_values
  outlier_substitutes <- response_var_object$outlier_substitutes
  
  outlier_values
  
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
                 predictors = data_covariates_clean,
                 outlier_positions = outlier_positions,
                 outlier_values = outlier_values,
                 outlier_substitutes = outlier_substitutes)
  
}


