# set the working directory
setwd("~/Documents/Doc-perso-2023/Job-Valeo/detection-project/yolov5")


# load libraries
#library(tidyverse)


source("./etl_pipeline/src/03_extract_detections.R")


# use create functions
main_dir <- "114823_20231212_104522_Record_data_INRIA"

extract_detection(main_dir = main_dir)
























