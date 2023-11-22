library(magick)
library(tidyverse)


setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

source("./src/impute_outliers.R")


source_data <- "./applications/final_dataset.csv"

data <- read.csv(file = source_data, sep = ",")

coefficient_iqr <- 3

data_lateral_error_object <- impute_outliers(x = data$lateral_error, 
                                             coefficient_iqr = coefficient_iqr, 
                                             method = c("interpolate", "mode", "median", "mean")[1])

data_longitudinal_error_object <- impute_outliers(x = data$longitudinal_error, 
                                                  coefficient_iqr = coefficient_iqr, 
                                                  method = c("interpolate", "mode", "median", "mean")[1])

data_haversine_error_object <- impute_outliers(x = data$haversine_error, 
                                               coefficient_iqr = coefficient_iqr, 
                                               method = c("interpolate", "mode", "median", "mean")[1])

data_lateral_error_abs_object <- impute_outliers(x = data$lateral_error_abs, 
                                                 coefficient_iqr = coefficient_iqr, 
                                                 method = c("interpolate", "mode", "median", "mean")[1])

data_longitudinal_error_abs_object <- impute_outliers(x = data$longitudinal_error_abs, 
                                                      coefficient_iqr = coefficient_iqr, 
                                                      method = c("interpolate", "mode", "median", "mean")[1])

data_haversine_error_abs_object <- impute_outliers(x = data$haversine_error_abs, 
                                                   coefficient_iqr = coefficient_iqr, 
                                                   method = c("interpolate", "mode", "median", "mean")[1])

data_latitude_error_object <- impute_outliers(x = data$latitude_error, 
                                              coefficient_iqr = coefficient_iqr, 
                                              method = c("interpolate", "mode", "median", "mean")[1])

data_longitude_error_object <- impute_outliers(x = data$longitude_error, 
                                               coefficient_iqr = coefficient_iqr, 
                                               method = c("interpolate", "mode", "median", "mean")[1])

data_heading_error_object <- impute_outliers(x = data$heading_error, 
                                             coefficient_iqr = coefficient_iqr, 
                                             method = c("interpolate", "mode", "median", "mean")[1])

data_latitude_error_abs_object <- impute_outliers(x = data$latitude_error_abs, 
                                                  coefficient_iqr = coefficient_iqr, 
                                                  method = c("interpolate", "mode", "median", "mean")[1])

data_longitude_error_abs_object <- impute_outliers(x = data$longitude_error_abs, 
                                                   coefficient_iqr = coefficient_iqr, 
                                                   method = c("interpolate", "mode", "median", "mean")[1])

data_heading_error_abs_object <- impute_outliers(x = data$heading_error_abs, 
                                                 coefficient_iqr = coefficient_iqr, 
                                                 method = c("interpolate", "mode", "median", "mean")[1])

outlier_positions <- c(data_lateral_error_object$outlier_positions,
                       data_longitudinal_error_object$outlier_positions,
                       data_haversine_error_object$outlier_positions,
                       data_lateral_error_abs_object$outlier_positions,
                       data_longitudinal_error_abs_object$outlier_positions,
                       data_haversine_error_abs_object$outlier_positions,
                       data_latitude_error_object$outlier_positions,
                       data_longitude_error_object$outlier_positions,
                       data_heading_error_object$outlier_positions,
                       data_latitude_error_abs_object$outlier_positions,
                       data_longitude_error_abs_object$outlier_positions,
                       data_heading_error_abs_object$outlier_positions)

outlier_positions <- sort(unique(outlier_positions))


source <- "./../../20231026_094602_Record_data_INRIA/images"

destination <- "./../../20231026_094602_Record_data_INRIA/extracted_images"


image_full_names <- list.files(source, pattern = '*.jpg$', full.names = FALSE)

image_full_names_to_extract <- image_full_names[outlier_positions]

for (image_name in image_full_names_to_extract){
  img <- image_read(path = paste0(source, "/", image_name))
  
  image_write(image = img, path = paste0(destination, "/", image_name), format = "jpg")
}

