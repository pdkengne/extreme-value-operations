# load created function
save_model_aic <- function(main_dir, 
                           response_var,
                           response_abs = FALSE,
                           variable = ~1,
                           scale_predictors = TRUE,
                           coefficient_iqr = Inf, 
                           remove_outliers = FALSE,
                           method = c("interpolate", "mode", "median", "mean")[1]){
  # main_dir:
  # response_var:
  # response_abs:
  # variable:
  # scale_predictors:
  # coefficient_iqr:
  # remove_outliers:
  # method:
  
  
  main_aic_dir <- file.path(main_dir, "aic")
  
  folder_name <- as.character(variable)[-1]
  
  output_csv_folder <- file.path(main_aic_dir, folder_name)
  
  if (!dir.exists(output_csv_folder)){
    dir.create(output_csv_folder)
  }
  
  output_csv_file <- file.path(output_csv_folder, "fitted_models_information.csv")
  
  input_csv_file <- file.path(main_dir, "merged_data.csv")
  
  data_object <- transform_data(data_path = input_csv_file, 
                                response_var = response_var,
                                response_abs = response_abs,
                                scale_predictors = scale_predictors,
                                coefficient_iqr = coefficient_iqr, 
                                remove_outliers = remove_outliers,
                                method = method)
  
  x <- data_object$response
  
  data <- data_object$predictors
  
  models <- make_models(variable)
  
  model_infos <- calculate_model_aic(x = x, data = data, models = models)
  
  write.csv(x = model_infos, file = output_csv_file, row.names = FALSE)
  
}

