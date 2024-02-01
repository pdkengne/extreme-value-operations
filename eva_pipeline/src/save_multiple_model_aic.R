source("./eva_pipeline/src/read_data.R")
source("./eva_pipeline/src/load_functions.R")

load_functions()

# load created function
save_multiple_model_aic <- function(main_dir, 
                                   response_var_vector,
                                   response_abs = FALSE,
                                   nrow_skip = 0,
                                   variable_vector = c(~1),
                                   scale_predictors = TRUE,
                                   coefficient_iqr = Inf, 
                                   iterate = 1,
                                   remove_outliers = FALSE,
                                   method = c("interpolate", "mode", "median", "mean")[1]){
  # main_dir:
  # response_var_vector:
  # response_abs:
  # nrow_skip:
  # variable_vector:
  # scale_predictors:
  # coefficient_iqr:
  # iterate:
  # remove_outliers:
  # method:
  
  input_csv_file <- file.path(main_dir, "merged_data.csv")
  
  dataset <- read_data(input_csv_file, nrow_skip = nrow_skip)
  
  lapply(response_var_vector, function(response_var){
    data_object <- transform_data(data = dataset, 
                                  response_var = response_var,
                                  response_abs = response_abs,
                                  scale_predictors = scale_predictors,
                                  coefficient_iqr = coefficient_iqr, 
                                  iterate = iterate,
                                  remove_outliers = remove_outliers,
                                  method = method)
    
    x <- data_object$response
    
    data <- data_object$predictors
    
    main_aic_dir <- file.path(main_dir, "aic")
    main_aic_dir <- file.path(main_aic_dir, response_var)
    
    lapply(variable_vector, function(variable){
      folder_name <- as.character(variable)[-1]
      
      output_csv_folder <- file.path(main_aic_dir, folder_name)
      
      if (!dir.exists(output_csv_folder)){
        dir.create(output_csv_folder, recursive = TRUE)
      }
      
      output_csv_file <- file.path(output_csv_folder, "fitted_models_information.csv")
      
      models <- make_models(variable)
      
      model_infos <- calculate_model_aic(x = x, data = data, models = models)
      
      write.csv(x = model_infos, file = output_csv_file, row.names = FALSE)
      
    })
    
  })
  
  
  
}

