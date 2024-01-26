# set the working directory
setwd("~/Documents/Doc-perso-2023/Job-Valeo/detection-project/yolov5")


# load libraries
#library(tidyverse)

source("./etl_pipeline/src/01_split_images.R")


# use create functions
main_dir <- "./105635_20231212_101549_Record_data_INRIA"

split_several_jpg_images(main_dir = main_dir, split_size = 400)



















