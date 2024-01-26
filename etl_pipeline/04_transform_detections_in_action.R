# set the working directory
setwd("~/Documents/Doc-perso-2023/Job-Valeo/detection-project/yolov5")


# load libraries
#library(tidyverse)


source("./etl_pipeline/src/04_transform_detections.R")


# use create functions
main_dir <- "./105635_20231212_101549_Record_data_INRIA"

transform_detections(main_dir = main_dir)
























