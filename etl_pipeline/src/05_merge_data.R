library(tidyverse)
library(caret)


# load created function
merge_data <- function(main_dir){
  # main_dir:
  
  output_csv_file <- file.path(main_dir, "merged_data.csv")
  
  if (file.exists(output_csv_file)){
    print(paste("The following file already exists:", output_csv_file))
  }
  else{
    input_csv_path_transformed_detections <- file.path(main_dir, "transformed_detections.csv")
    transformed_detections_file <- read.csv(file = input_csv_path_transformed_detections, header = TRUE, sep = ",")
    
    input_csv_path_localization_errors <- file.path(main_dir, "localization_errors.csv")
    localization_errors_file <- read.csv(file = input_csv_path_localization_errors, header = TRUE, sep = ";")
    
    input_csv_path_Gnss_imar <- file.path(main_dir, "Gnss_imar.csv")
    Gnss_imar_file <- read.csv(file = input_csv_path_Gnss_imar, header = TRUE, sep = ",")
    
    input_csv_path_Gnss_map_matching <- file.path(main_dir, "Gnss_map_matching.csv")
    Gnss_map_matching_file <- read.csv(file = input_csv_path_Gnss_map_matching, header = TRUE, sep = ",")
    
    match_id <- is.element(el = localization_errors_file$timestamp,
                           set = Gnss_imar_file$timestamp)
    
    transformed_detections_file <- transformed_detections_file[match_id, ]
    
    main_csv_file_trans_df <- dplyr::inner_join(x = localization_errors_file,
                                                y = Gnss_imar_file,
                                                by = "timestamp",
                                                copy = FALSE,
                                                suffix = c(".x", ".y"),
                                                keep = NULL)
    
    selected_vars <- c("timestamp", "latitude", "longitude", "lateral_error", "longitudinal_error")
    main_csv_file_trans_df <- main_csv_file_trans_df %>% select(all_of(selected_vars))
    
    main_csv_file_trans_df$latitude_error <- Gnss_imar_file$latitude - Gnss_map_matching_file$latitude
    main_csv_file_trans_df$longitude_error <- Gnss_imar_file$longitude - Gnss_map_matching_file$longitude
    
    main_csv_file_trans_df <- main_csv_file_trans_df %>% mutate(velocity_latitude = c(NA, diff(latitude)/diff(timestamp)),
                                                                velocity_longitude = c(NA, diff(longitude)/diff(timestamp)),
                                                                velocity = c(NA, sqrt(na.omit(velocity_latitude)^2 + na.omit(velocity_longitude)^2)))
    
    main_csv_file_trans_df <- cbind(main_csv_file_trans_df, transformed_detections_file)
    main_csv_file_trans_df <- main_csv_file_trans_df[-1, ]
    
    write.csv(x = main_csv_file_trans_df, file = output_csv_file, row.names = FALSE)
    
    # main_csv_file_trans_df
  }
  
}


