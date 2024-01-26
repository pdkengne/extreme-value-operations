# load libraries
# library(tidyverse)
# library(ggplot2)
# library(magick)


# load created function
create_several_folder_names <- function(base_folder_name = "folder", suffixes = NULL, separator = ""){
  # base_folder_name:
  # suffixes:
  # separator:
  
  folder_names <- paste(base_folder_name, suffixes, sep = separator)
  
  folder_names
}


# load created function
create_several_folders <- function(folder_names){
  # folder_names: a vector which contains the names of folders to create if not exists
  
  sapply(folder_names, function(folder){
    if (!dir.exists(folder)){
      dir.create(folder)
    } 
  })
  
}


# load created function
split_several_jpg_images <- function(main_dir, split_size = 400){
  # main_dir:
  # split_size:
  
  image_path = "images"
  
  full_image_path <- file.path(main_dir, image_path)
  
  image_full_names <- list.files(full_image_path, pattern = '*.jpg$', full.names = FALSE)
  
  n <- length(image_full_names)
  
  nsplits <- ceiling(n/split_size)
  
  folder_split_images_without_detections <- file.path(main_dir, "split_images_without_detections")
  folder_split_images_with_detections <- file.path(main_dir, "split_images_with_detections")
  folder_split_images_detection_coordinates <- file.path(main_dir, "split_images_detection_coordinates")
  
  create_several_folders(folder_split_images_without_detections)
  create_several_folders(folder_split_images_with_detections)
  create_several_folders(folder_split_images_detection_coordinates)
  
  full_images_without_detections_paths <- create_several_folder_names(base_folder_name = file.path(folder_split_images_without_detections, "split"), 
                                                                      suffixes = 1:nsplits, separator = "_")
  
  full_images_with_detections_paths <- create_several_folder_names(base_folder_name = file.path(folder_split_images_with_detections, "split"), 
                                                                   suffixes = 1:nsplits, separator = "_")
  
  full_images_detection_coordinates_paths <- create_several_folder_names(base_folder_name = file.path(folder_split_images_detection_coordinates, "split"), 
                                                                         suffixes = 1:nsplits, separator = "_")
  
  create_several_folders(folder_names = full_images_without_detections_paths)
  create_several_folders(folder_names = full_images_with_detections_paths)
  create_several_folders(folder_names = full_images_detection_coordinates_paths)
  
  x <- 1:n
  
  split_indicators <- as.numeric(ggplot2::cut_number(x = x, n = nsplits))
  
  split_positions <- lapply(1:nsplits, function(split) which(split_indicators == split))
  
  sapply(1:nsplits, function(split){
    destination <- full_images_without_detections_paths[split]
    
    image_full_names_to_extract <- image_full_names[split_positions[[split]]]
    
    for (image_name in image_full_names_to_extract){
      img <- magick::image_read(path = file.path(full_image_path, image_name))
      
      magick::image_write(image = img, path = file.path(destination, image_name), format = "jpg")
    }
    
  })
  
}


