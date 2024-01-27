
# load created function
extract_multiple_model_aic <- function(main_dir, response_var_vector){
  # main_dir:
  # response_var_vector:
  
  lapply(response_var_vector, function(response_var){
    main_csv_dir <- file.path(main_dir, "aic")
    main_csv_dir <- file.path(main_csv_dir, response_var)
    
    output_csv_file <- file.path(main_dir, paste(response_var, "model_aic.csv"))
    
    if (file.exists(output_csv_file)){
      print(paste("The following file already exists:", output_csv_file))
    }
    else{
      all_csv_dir <- list.dirs(path = main_csv_dir, full.names = TRUE, recursive = FALSE)
      
      all_csv_files_list <- lapply(all_csv_dir, function(folder){
        
        csv_files_names <- list.files(folder, pattern = '*.csv', full.names = FALSE)
        
        csv_files_list <- lapply(csv_files_names, function(file){
          file_path <- file.path(folder, file)
          csv_file <- read.csv(file = file_path, header = TRUE, sep = ",")
          csv_file$variable <- basename(folder)
          
          csv_file
          
        })
        
        csv_files_df <- do.call(rbind, csv_files_list)
        
        csv_files_df
        
      })
      
      all_csv_files_df <- do.call(rbind, all_csv_files_list)
      
      write.csv(x = all_csv_files_df, file = output_csv_file, row.names = FALSE)
      
      # all_csv_files_df
      
    }
    
  })
  
}


