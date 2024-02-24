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
transform_data <- function(detections, classes){
  
  # create columns associated with bounding boxes center and area
  width <- 1280
  height <- 800
  image_area <- width*height
  
  detections <- detections %>% mutate(xcenter = (xmin + xmax)/(2*width))
  detections <- detections %>% mutate(ycenter = (ymin + ymax)/(2*height))
  detections <- detections %>% mutate(area = (xmax - xmin)*(ymax - ymin)/image_area)
  
  # extract the classes of detected objects
  detected_object_classes <- unique(detections$name)
  detected_object_classes <- stringr::str_replace_all(string = detected_object_classes, pattern  = " ", replacement = "_")
  detected_object_classes <- stringr::str_replace_all(string = detected_object_classes, pattern  = "\\(.*\\)", replacement = "group")
  
  # create columns associated with: object_total
  detections_size <- tapply(X = detections$class,
                            INDEX = list(detections$name),
                            FUN = length)
  
  str_object_total <- paste("object_total", detected_object_classes, sep = "_")
  detections_size_df <- data.frame(t(detections_size))
  names(detections_size_df) <- str_object_total
  
  # create columns associated with: area_total
  detections_area <- tapply(X = detections$area,
                            INDEX = list(detections$name),
                            FUN = sum)
  
  str_area_total <- paste("area_total", detected_object_classes, sep = "_")
  detections_area_df <- data.frame(t(detections_area))
  names(detections_area_df) <- str_area_total
  
  # create columns associated with: xcenter_mean
  detections_xcenter_mean <- tapply(X = detections$xcenter,
                                    INDEX = list(detections$name),
                                    FUN = mean)
  
  str_xcenter_mean <- paste("xcenter_mean", detected_object_classes, sep = "_")
  detections_xcenter_mean_df <- data.frame(t(detections_xcenter_mean))
  names(detections_xcenter_mean_df) <- str_xcenter_mean
  
  # create columns associated with: xcenter_sd
  detections_xcenter_sd <- tapply(X = detections$xcenter,
                                  INDEX = list(detections$name),
                                  FUN = sd)
  
  str_xcenter_sd <- paste("xcenter_sd", detected_object_classes, sep = "_")
  detections_xcenter_sd_df <- data.frame(t(detections_xcenter_sd))
  names(detections_xcenter_sd_df) <- str_xcenter_sd
  
  # create columns associated with: ycenter_mean
  detections_ycenter_mean <- tapply(X = detections$ycenter,
                                    INDEX = list(detections$name),
                                    FUN = mean)
  
  str_ycenter_mean <- paste("ycenter_mean", detected_object_classes, sep = "_")
  detections_ycenter_mean_df <- data.frame(t(detections_ycenter_mean))
  names(detections_ycenter_mean_df) <- str_ycenter_mean
  
  # create columns associated with: ycenter_sd
  detections_ycenter_sd <- tapply(X = detections$ycenter,
                                  INDEX = list(detections$name),
                                  FUN = sd)
  
  str_ycenter_sd <- paste("ycenter_sd", detected_object_classes, sep = "_")
  detections_ycenter_sd_df <- data.frame(t(detections_ycenter_sd))
  names(detections_ycenter_sd_df) <- str_ycenter_sd
  
  # concatenate all tables
  detections_df <- cbind(detections_size_df,
                         detections_area_df,
                         detections_xcenter_mean_df,
                         detections_xcenter_sd_df,
                         detections_ycenter_mean_df,
                         detections_ycenter_sd_df)
  
  # extract the classes of undetected objects
  undetected_object_classes <- classes[!(classes %in% detected_object_classes)]
  
  # create columns associated with undetected objects
  str_object_total <- paste("object_total", undetected_object_classes, sep = "_")
  str_area_total <- paste("area_total", undetected_object_classes, sep = "_")
  str_xcenter_mean <- paste("xcenter_mean", undetected_object_classes, sep = "_")
  str_xcenter_sd <- paste("xcenter_sd", undetected_object_classes, sep = "_")
  str_ycenter_mean <- paste("ycenter_mean", undetected_object_classes, sep = "_")
  str_ycenter_sd <- paste("ycenter_sd", undetected_object_classes, sep = "_")
  
  undefined_variables <- c(str_object_total, 
                           str_area_total,
                           str_xcenter_mean,
                           str_xcenter_sd,
                           str_ycenter_mean,
                           str_ycenter_sd)
  
  additional_data <- data.frame("X" = rep(x = "NA", times = length(undefined_variables)))
  additional_data <- data.frame(t(additional_data))
  names(additional_data) <- undefined_variables
  
  # create columns associated with undetected and detected objects
  final_detections_df <- cbind(detections_df, additional_data)
  
  final_detections_df
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
    all_object_classes <- c("tree (group)","airplane","truck","car","gas station","tree","electric pole",
                            "helicopter","person","person (group)","tenement","bench (group)","street light",
                            "traffic sign","fence","traffic light","house (group)","tunnel","bus","house",
                            "bird","parking meter","tunnel entrance","bird (group)","bench","special building",
                            "car (group)","fire hydrant","motorcycle","scooter","stop sign","train","dog","bicycle","boat")
    
    all_object_classes <- stringr::str_replace_all(string = all_object_classes, pattern  = " ", replacement = "_")
    all_object_classes <- stringr::str_replace_all(string = all_object_classes, pattern  = "\\(.*\\)", replacement = "group")
    
    main_csv_file <- read.csv(file = input_csv_path, header = TRUE, sep = ",")
    
    file_names <- sort(x = main_csv_file$file, decreasing = FALSE)
    unique_file_names <- unique(file_names)
    
    main_csv_file_trans_list <- lapply(unique_file_names, function(file){
      position <- which(file_names == file)
      main_csv_file_crop <- main_csv_file %>% slice(position) %>% select(!file)
      
      csv_file_trans <- transform_data(detections = main_csv_file_crop, classes = all_object_classes)
      
      csv_file_trans$file <- file
      
      csv_file_trans
    })
    
    main_csv_file_trans_df <- do.call(rbind, main_csv_file_trans_list)
    
    write.csv(x = main_csv_file_trans_df, file = output_csv_file, row.names = FALSE)
    
    # main_csv_file_trans_df
  }
  
}


