library(caret)

get_one_hot_encoded_and_standard_scaled_data <- function(data, newdata = NULL){
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
  
  # get data one hot encoder
  data_one_hot_encoder <- caret::dummyVars(formula = " ~ .", 
                                           sep = NULL, 
                                           data = data)
  
  # get one hot encoded and scaled data
  one_hot_encoded_scaled_data <- data.frame(predict(object = data_one_hot_encoder, 
                                                    newdata = standard_scaled_data))
     
  one_hot_encoded_scaled_data
}


# # example 1
# 
# data <- data.frame(id=c(10,20,30,40,50),
#                    gender=c('male','female','female','male','female'),
#                    mood=c('happy','sad','happy','sad','happy'),
#                    outcome=c(1,1,0,0,0))
# 
# data
# 
# result <- get_one_hot_encoded_and_standard_scaled_data(data = data, newdata = NULL)
# 
# result
# 
# result <- get_one_hot_encoded_and_standard_scaled_data(data = data, newdata = data[1, ])
# 
# result
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
# result <- get_one_hot_encoded_and_standard_scaled_data(data = data, newdata = NULL)
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
# result <- get_one_hot_encoded_and_standard_scaled_data(data = data, newdata = newdata)
# 
# result
