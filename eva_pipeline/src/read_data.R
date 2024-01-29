source("./eva_pipeline/src/load_functions.R")

load_functions()


# load functions
read_data <- function(data_path, nrow_skip = 0){
  # data_path:
  # nrow_skip:
  
  # data <- read.csv(file = data_path, header = TRUE, sep = ",", dec = ".")
  
  
  data <- vroom::vroom(file = data_path,
                       delim = NULL,
                       col_names = TRUE,
                       col_types = NULL,
                       col_select = NULL,
                       id = NULL,
                       skip = 0,
                       n_max = Inf,
                       na = c("", "NA"),
                       quote = "\"",
                       comment = "",
                       skip_empty_rows = TRUE,
                       trim_ws = TRUE,
                       escape_double = TRUE,
                       escape_backslash = FALSE,
                       guess_max = 100,
                       show_col_types = TRUE,
                       .name_repair = "unique")
    
  
  if (nrow_skip + 1 > nrow(data) | nrow_skip < 0){
    stop(paste("Sorry, the argument nrow_skip must be a positive integer smaller than or equal to", nrow(data)))
  }
  else{
    indexes <- seq(from = nrow_skip + 1, to = nrow(data), by = 1)
    
    data <- data |> slice(indexes)
  }
  
  data
  
}


