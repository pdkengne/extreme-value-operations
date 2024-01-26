# set the working directory
setwd("~/Documents/Doc-perso-2023/Job-Valeo/detection-project/yolov5")


# load libraries
#library(tidyverse)


source("./etl_pipeline/src/05_merge_data.R")


# use create functions
main_dir <- "./20231026_094602_Record_data_INRIA"

merge_data(main_dir = main_dir)
























