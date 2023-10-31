library(caret)

get_one_hot_encoded_data <- function(data, newdata = NULL){
  # data: A data frame with the predictors of interest
  # newdata: a data frame of new observations
  
  # Convert all character columns to factor
  data <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
  
  # select the data frame of new observations to consider
  if (is.null(newdata)){
    newdata <- data
  }
  
  # get data one hot encoder
  data_one_hot_encoder <- caret::dummyVars(formula = " ~ .", 
                                           sep = NULL, 
                                           data = data)
  
  # get one hot encoded data
  one_hot_encoded_data <- data.frame(predict(object = data_one_hot_encoder, 
                                             newdata = newdata))
  
  one_hot_encoded_data
}


# # example 1
# 
# data <- data.frame(cbind(1:10, 1:10))
# 
# data
# 
# results <- get_one_hot_encoded_data(data = data, newdata = NULL)
# 
# results
# 
# 
# # example 2
# 
# data <- data.frame(id=c(10,20,30,40,50),
#                    gender=c('male','female','female','male','female'),
#                    mood=c('happy','sad','happy','sad','happy'),
#                    outcome=c(1,1,0,0,0))
# 
# data
# 
# result <- get_one_hot_encoded_data(data = data, newdata = NULL)
# 
# result
# 
# result <- get_one_hot_encoded_data(data = data, newdata = data[1, ])
# 
# result
# 
# 
# # example 3
# 
# data <- iris
# 
# head(data)
# 
# tail(data)
# 
# result <- get_one_hot_encoded_data(data = data, newdata = NULL)
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
# result <- get_one_hot_encoded_data(data = data, newdata = newdata)
# 
# result
