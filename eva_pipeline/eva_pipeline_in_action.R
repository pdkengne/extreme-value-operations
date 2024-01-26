setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

source("./eva_pipeline/src/load_functions.R")

# example
load_functions()


source("./eva_pipeline/src/make_models.R")

# example
make_models(variable = ~1)

make_models(variable = ~age)


source("./eva_pipeline/src/transform_data.R")


source("./eva_pipeline/src/calculate_model_aic.R")


source("./eva_pipeline/src/save_model_aic.R")






# use created function




# load created function
extract_aic <- function(main_dir){
  # main_dir:
  
  main_aic_dir <- file.path(main_dir, "aic")
  output_csv_file <- file.path(main_dir, "extracted_model_aic.csv")
  
  if (file.exists(output_csv_file)){
    print(paste("The following file already exists:", output_csv_file))
  }
  else{
    all_aic_dir <- list.dirs(path = main_aic_dir, full.names = TRUE, recursive = FALSE)
    
    
  }
  
  
  
}








# use created function
setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

defaultW <- getOption("warn") 
options(warn = -1) 


main_dir  <- "./04_experimentation_01"

response_var <- "lateral_error"

save_model_aic(main_dir = main_dir,
                 response_var = response_var,
                 variable = ~object, 
                 scale_predictors = TRUE,
                 coefficient_iqr = 9, 
                 remove_outliers = FALSE,
                 method = c("interpolate", "mode", "median", "mean")[1])


options(warn = defaultW)




















