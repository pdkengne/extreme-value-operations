# Written by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# read_data <- function(data_path, nrow_skip = 0, header = TRUE, sep = ",", dec = "."){
#   
#   print("read_data: ok")
#   
# }
# 
# 
# make_data_path <- function(main_dir, csv_file_name){
#   
#   print("make_data_path: ok")
#   
# }
# 
# 
# transform_data <- function(data, 
#                            response_var,
#                            response_abs = FALSE,
#                            scale_predictors = TRUE,
#                            coefficient_iqr = Inf, 
#                            remove_outliers = FALSE,
#                            method = c("interpolate", "mode", "median", "mean")[1]){
#   
#   print("transform_data: ok")
#   
# }
# 
# 
# make_models <- function(predictor_name = ~1){
#   
#   print("make_models: ok")
#   
# }
# 
# 
# calculate_models_aic <- function(transformed_data, models){
#   
#   print("calculate_model_aic: ok")
#   
# }
# 
# 
# 
# make_aic_path <- function(main_dir, predictor_name = ~1){
#   
#   print("make_aic_path: ok")
#   
# }
# 
# 
# 
# save_models_aic <- function(models_aic, aic_path){
#   
#   print("save_models_aic: ok")
#   
# }


#-------------------------------------------------------------------------------


divide_images <- function(main_dir, images_dir, batch_size = 400){
  
  print("split_several_jpg_images: ok")
  
}


detect_objects <- function(divided_images){
  
  print("detect_objects_from_jpg_images: ok")
  
}


extract_bounding_boxes <- function(detected_objects){
  
  print("extract_bounding_boxes: ok")
  
}


concatenate_bounding_boxes <- function(extracted_bounding_boxes){
  
  print("concatenate_bounding_boxes: ok")
  
}


transform_bounding_boxes <- function(concatenated_bounding_boxes){
  
  print("transform_bounding_boxes: ok")
  
}


merge_data <- function(transformed_bounding_boxes, 
                       ground_truth_data, 
                       estimated_data, 
                       error_data){
  
  print("merge_bounding_boxes: ok")
  
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
  
  tar_target(name = images_dir, command = "images_dir", pattern = NULL),
  
  tar_target(name = batch_size, command = 400, pattern = NULL),
  
  tar_target(name = divided_images, 
             command = divide_images(main_dir = main_dir, 
                                     images_dir = images_dir,
                                     batch_size = batch_size), 
             pattern = NULL),
  
  
  tar_target(name = detected_objects, 
             command = detect_objects(divided_images = divided_images), 
             pattern = NULL),
  
  
  tar_target(name = extracted_bounding_boxes, 
             command = extract_bounding_boxes(detected_objects = detected_objects), 
             pattern = NULL),
  
  
  tar_target(name = concatenated_bounding_boxes, 
             command = concatenate_bounding_boxes(extracted_bounding_boxes = extracted_bounding_boxes), 
             pattern = NULL),
  
  
  tar_target(name = transformed_bounding_boxes, 
             command = transform_bounding_boxes(concatenated_bounding_boxes = concatenated_bounding_boxes), 
             pattern = NULL),
  
  
  tar_target(name = ground_truth_data, command = "ground_truth_data.csv", pattern = NULL),
  
  tar_target(name = estimated_data, command = "estimated_data.csv", pattern = NULL),
  
  tar_target(name = error_data, command = "error_data.csv", pattern = NULL),
  
  
  tar_target(name = merged_data, 
             command = merge_data(transformed_bounding_boxes = transformed_bounding_boxes, 
                                  ground_truth_data = ground_truth_data, 
                                  estimated_data = estimated_data, 
                                  error_data = error_data), 
             pattern = NULL)
  
  
  
  
  
  
  
  # tar_target(name = main_dir, command = "main_dir", pattern = NULL),
  # 
  # tar_target(name = csv_file_name, command = "file.csv", pattern = NULL),
  # 
  # tar_target(name = nrow_skip, command = c(0), pattern = NULL),
  # 
  # 
  # tar_target(name = data_path, 
  #            command = make_data_path(main_dir = main_dir, 
  #                                     csv_file_name = csv_file_name), 
  #            # format = "file",
  #            pattern = NULL),
  # 
  # 
  # tar_target(name = data, 
  #            command = read_data(data_path = data_path, 
  #                                nrow_skip = nrow_skip, 
  #                                header = TRUE, 
  #                                sep = ",", 
  #                                dec = "."),
  #            pattern = NULL),
  # 
  # 
  # tar_target(name = response_var, command = "response_var_name", pattern = NULL),
  # 
  # 
  # tar_target(name = transformed_data, 
  #            command = transform_data(data = data, 
  #                                     response_var = response_var,
  #                                     response_abs = TRUE,
  #                                     scale_predictors = TRUE,
  #                                     coefficient_iqr = 9, 
  #                                     remove_outliers = FALSE,
  #                                     method = c("interpolate", "mode", "median", "mean")[1]), 
  #            pattern = NULL),
  # 
  # 
  # tar_target(name = predictor_name, command = ~var_name, pattern = NULL),
  # 
  # 
  # tar_target(name = models, 
  #            command = make_models(predictor_name = predictor_name), 
  #            pattern = NULL),
  # 
  # 
  # tar_target(name = models_aic, 
  #            command = calculate_models_aic(transformed_data = transformed_data, 
  #                                           models = models), 
  #            pattern = NULL),
  # 
  # 
  # tar_target(name = aic_path, 
  #            command = make_aic_path(main_dir = main_dir, 
  #                                    predictor_name = predictor_name), 
  #            pattern = NULL),
  # 
  # 
  # tar_target(name = saved_aic, 
  #            command = save_models_aic(models_aic = models_aic, 
  #                                      aic_path = aic_path), 
  #            pattern = NULL)
  
  
  
  
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
