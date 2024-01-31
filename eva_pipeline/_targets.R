# Written by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

read_data <- function(data_path, nrow_skip = 0, header = TRUE, sep = ",", dec = "."){
  
  print("read_data: ok")
  
}


make_data_path <- function(main_dir, csv_file_name){
  
  print("make_data_path: ok")
  
}


transform_data <- function(data, 
                           response_vars,
                           response_abs = FALSE,
                           scale_predictors = TRUE,
                           coefficient_iqr = Inf, 
                           remove_outliers = FALSE,
                           method = c("interpolate", "mode", "median", "mean")[1]){
  
  print("transform_data: ok")
  
}


make_models <- function(predictor_names = ~1){
  
  print("make_models: ok")
  
}


calculate_models_aic <- function(transformed_data, models){
  
  print("calculate_model_aic: ok")
  
}



make_aic_path <- function(main_dir, predictor_names = ~1){
  
  print("make_aic_path: ok")
  
}



save_models_aic <- function(models_aic, aic_path){
  
  print("save_models_aic: ok")
  
}







# Set target options:
tar_option_set(
  packages = c("dplyr", "ggplot2", "readr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# Configure the backend of tar_make_clustermq() (recommended):
options(clustermq.scheduler = "multicore")

# Configure the backend of tar_make_future() (optional):
future::plan(future.callr::callr)

# Load the R scripts with your custom functions:
# lapply(list.files("R", full.names = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint



# Replace the target list below with your own:
list(
  tar_target(name = main_dir, command = "main_dir", pattern = NULL),
  
  tar_target(name = csv_file_name, command = "file.csv", pattern = NULL),
  
  tar_target(name = nrow_skip, command = c(0), pattern = NULL),
  
  
  tar_target(name = data_path, 
             command = make_data_path(main_dir = main_dir, 
                                      csv_file_name = csv_file_name), 
             # format = "file",
             pattern = NULL),
  
  
  tar_target(name = data, 
             command = read_data(data_path = data_path, 
                                 nrow_skip = nrow_skip, 
                                 header = TRUE, 
                                 sep = ",", 
                                 dec = "."),
             pattern = NULL),
  
  
  tar_target(name = response_vars, command = c("response_var_name_1", "response_var_name_2"), 
             pattern = NULL),
  
  
  tar_target(name = transformed_data, 
             command = transform_data(data = data, 
                                      response_vars = response_vars,
                                      response_abs = TRUE,
                                      scale_predictors = TRUE,
                                      coefficient_iqr = 9, 
                                      remove_outliers = FALSE,
                                      method = c("interpolate", "mode", "median", "mean")[1]), 
             pattern = cross(predictor_names, response_vars)),
  
  
  tar_target(name = predictor_names, command = c(~var_name_1, ~var_name_2, ~var_name_3), 
             pattern = NULL),
  
  
  tar_target(name = models, 
             command = make_models(predictor_names = predictor_names), 
             pattern = cross(predictor_names, response_vars)),
  
  
  tar_target(name = models_aic, 
             command = calculate_models_aic(transformed_data = transformed_data, 
                                            models = models), 
             pattern = map(models, transformed_data)),
  
  
  tar_target(name = aic_path, 
             command = make_aic_path(main_dir = main_dir, 
                                     predictor_names = predictor_names), 
             pattern = cross(predictor_names, response_vars)),
  
  
  tar_target(name = saved_aic, 
             command = save_models_aic(models_aic = models_aic, 
                                       aic_path = aic_path),
             pattern = map(models_aic, aic_path))
  
  #-----------------------------------------------------------------------------

  
  
  
  
  
  # tar_target(model, fit_model(data)),
  # tar_target(plot, plot_model(model, data)),
  # 
  # tar_target(x, c(10, 20, 30)),
  # tar_target(y, c(1, 2, 3)),
  # tar_target(z, c(2, 4, 6)),
  # tar_target(
  #   resultat_1,
  #   data.frame(argument_1 = x, argument_2 = y, res = simulation(x, y)),
  #   pattern = map(x, y)),
  
  # tar_target(
  #   resultat_2,
  #   data.frame(argument_1 = x, argument_2 = y, res = simulation(x, y)),
  #   pattern = cross(x, y)),
  
  # tar_target(
  #   resultat_3,
  #   data.frame(argument_1 = x, argument_2 = y, argument_3 = z, res = simulation(x, y, z)),
  #   pattern = cross(x, map(y, z)))
  
)
