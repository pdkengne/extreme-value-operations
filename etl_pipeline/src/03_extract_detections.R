# library(tidyverse)


# load created function
extract_detection <- function(main_dir){
  # main_dir:
  
  input_csv_path <- "split_images_detection_coordinates"
  output_csv_file <- file.path(main_dir, "extracted_detections.csv")
  
  if (file.exists(output_csv_file)){
    print(paste("The following file already exists:", output_csv_file))
  }
  else{
    main_csv_dir <- file.path(main_dir, input_csv_path)
    all_csv_dir <- list.dirs(path = main_csv_dir, full.names = TRUE, recursive = FALSE)
    
    all_csv_files_list <- lapply(all_csv_dir, function(folder){
      
      csv_files_names <- list.files(folder, pattern = '*.csv', full.names = FALSE)
      
      csv_files_list <- lapply(csv_files_names, function(file){
        file_path <- file.path(folder, file)
        csv_file <- read.csv(file = file_path, header = TRUE, sep = ",")
        csv_file$file <- file
        
        csv_file
        
      })
      
      csv_files_df <- do.call(rbind, csv_files_list)
      
      csv_files_df
      
    })
    
    all_csv_files_df <- do.call(rbind, all_csv_files_list)
    
    write.csv(x = all_csv_files_df, file = output_csv_file, row.names = FALSE)
    
    # all_csv_files_df
  }
  
}


