# set the working directory
setwd("~/Documents/Doc-perso-2023/Job-Valeo/detection-project/yolov5")

library(reticulate)

use_virtualenv("./yolov5_venv")


source_python("./etl_pipeline/src/02_detect_objects.py")

# use create functions
main_dir <- "./105635_20231212_101549_Record_data_INRIA"

detect_objects_from_jpg_images(main_dir = main_dir)

use_python("/usr/bin/python3", required = NULL)
















