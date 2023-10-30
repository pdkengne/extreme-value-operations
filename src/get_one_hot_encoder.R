library(caret)

get_one_hot_encoder <- function(data){
  # data: A data frame with the predictors of interest
  
  # Convert all character columns to factor
  data <- as.data.frame(unclass(data), stringsAsFactors = TRUE)
  
  output <- caret::dummyVars(formula = " ~ .", sep = NULL, data = data)
  
  output
}


# # example 1
# 
# data <- data.frame(id=c(10,20,30,40,50),
#                    gender=c('male','female','female','male','female'),
#                    mood=c('happy','sad','happy','sad','happy'),
#                    outcome=c(1,1,0,0,0))
# 
# str(data)
# 
# result <- get_one_hot_encoder(data = data)
# 
# result
# 
# transformed_data <- data.frame(predict(object = result, newdata = data))
# 
# transformed_data
# 
# 
# transformed_data <- data.frame(predict(object = result, newdata = data[1, ], contrasts = FALSE))
# 
# transformed_data
