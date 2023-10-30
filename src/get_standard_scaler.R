library(caret)

get_standard_scaler <- function(data, method = c("center", "scale")){
  # data: a matrix or dataframe
  # method: a character vector specifying the type of processing. Possible values are
  # c("BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "pca", "ica", "medianImpute", bagImpute", "corr")
  
  output <- caret::preProcess(x = data, method = method)
  
  output
}


# # example 1
# 
# data <- data.frame(cbind(1:10, 1:10))
# 
# data
# 
# results <- get_standard_scaler(data = data, method = c("center", "scale"))
# 
# results
# 
# names(results)
# 
# results$mean
# 
# results$std

