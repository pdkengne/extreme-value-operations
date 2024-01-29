# load functions
read_data <- function(data_path){
  # data_path:
  
  data <- read.csv(file = data_path, header = TRUE, sep = ",", dec = ".")
  
  data
  
}


