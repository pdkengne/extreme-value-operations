library(tidyverse)
library(caret)


# load created function
get_one_hot_encoded_data <- function(data, newdata = NULL){
  # data: A data frame with the predictors of interest
  # newdata: a data frame of new observations
  
  # Convert all character columns to factor
  data <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
  
  # select the data frame of new observations to consider
  if (is.null(newdata)){
    newdata <- data
  }
  
  # get data one hot encoder
  data_one_hot_encoder <- caret::dummyVars(formula = " ~ .", 
                                           sep = "_", 
                                           data = data)
  
  # get one hot encoded data
  one_hot_encoded_data <- data.frame(predict(object = data_one_hot_encoder, 
                                             newdata = newdata))
  
  one_hot_encoded_data
}



# load created function
transform_detections <- function(main_dir){
  # main_dir:
  
  input_csv_path <- file.path(main_dir, "extracted_detections.csv")
  output_csv_file <- file.path(main_dir, "transformed_detections.csv")
  
  if (file.exists(output_csv_file)){
    print(paste("The following file already exists:", output_csv_file))
  }
  else{
    main_csv_file <- read.csv(file = input_csv_path, header = TRUE, sep = ",")
    
    width <- 1280
    height <- 800
    image_area <- width*height
    
    main_csv_file_crop <- main_csv_file %>% select(!file)
    main_csv_file_crop <- main_csv_file_crop %>% mutate(horizontal = ifelse(test = (xmin + xmax)/2 > width/2, yes = "right", no = "left"))
    main_csv_file_crop <- main_csv_file_crop %>% mutate(vertical = ifelse(test = (ymin + ymax)/2 > height/2, yes = "up", no = "down"))
    main_csv_file_crop <- main_csv_file_crop %>% mutate(area = (xmax - xmin)*(ymax - ymin)/image_area)
    main_csv_file_crop <- main_csv_file_crop %>% select(name, horizontal, vertical, area)
    
    main_csv_file_crop_encoded <- get_one_hot_encoded_data(data = main_csv_file_crop, newdata = NULL)
    
    file_names <- sort(x = main_csv_file$file, decreasing = FALSE)
    unique_file_names <- unique(file_names)
    
    main_csv_file_trans_list <- lapply(unique_file_names, function(file){
      position <- which(file_names == file)
      csv_file_trans <- data.frame(t(apply(main_csv_file_crop_encoded[position, ], 2, sum)))
      csv_file_trans$object <- length(position)
      csv_file_trans$file <- file
      
      csv_file_trans
    })
    
    main_csv_file_trans_df <- do.call(rbind, main_csv_file_trans_list)
    
    write.csv(x = main_csv_file_trans_df, file = output_csv_file, row.names = FALSE)
    
    # main_csv_file_trans_df
  }
  
}


