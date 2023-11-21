library(caret)

get_standard_scaled_data <- function(data, newdata = NULL){
  # data: A data frame with the predictors of interest
  # newdata: a data frame of new observations
  
  # Convert all character columns to factor
  data <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
  
  # select the data frame of new observations to consider
  if (is.null(newdata)){
    newdata <- data
  }
  
  # get standard scaler
  standard_scaler <- caret::preProcess(x = data, 
                                       method = c("center", "scale"))
  
  # get standard scaled data 
  standard_scaled_data <- predict(object = standard_scaler, 
                                  newdata = newdata)
  
  standard_scaled_data
}


# # example 1
# 
# data <- data.frame(cbind(1:10, 1:10))
# 
# data
# 
# results <- get_standard_scaled_data(data = data, newdata = NULL)
# 
# results
# 
# 
# # example 2
# 
# data <- iris
# 
# head(data)
# 
# tail(data)
# 
# result <- get_standard_scaled_data(data = data, newdata = NULL)
# 
# head(result)
# 
# tail(result)
# 
# str(result)
# 
# newdata <- data[c(1, 100, 150), ]
# 
# newdata
# 
# result <- get_standard_scaled_data(data = data, newdata = newdata)
# 
# result
