# library(forecast)
# library(mice)

source("./src/calculate_mode.R")

impute_outliers <- function(x, 
                            coefficient_iqr = 1.5, 
                            iterate = 10,
                            method = c("interpolate", "mode", "median", "mean")[1]){
  # x: vector of observations
  # method: indicates the method of outlier values imputation
  # iterate: indicates the number of iterations required
  # coefficient_iqr: indicates the multiple of interquartile range to extend out from the box bounds
  
  # create an empty output object
  output <- list()
  
  # create a boxplot object
  boxplot_object <- boxplot.stats(x, coef = coefficient_iqr)
  
  # extract the outlier values
  outlier_values <- boxplot_object$out
  
  # get the outlier positions
  outlier_positions <- which(x %in% outlier_values)
  
  # replace outlier values with NA
  imputed_data <- x
  imputed_data[outlier_positions] <- NA
  
  # check the correctness of the iterate argument
  if (iterate <= 0){
    stop("Please enter appropriate 'iterate' argument. Example: 'iterate = k', where k > 0")
  }
  
  # initialize the counter
  k <- 1
  current_outlier_positions <- outlier_positions
  
  while (k < ceiling(iterate) & length(current_outlier_positions) > 0){
    # update the time series
    y <- imputed_data
    
    # create a boxplot object
    boxplot_object <- boxplot.stats(y, coef = coefficient_iqr)
    
    # extract the outlier values
    current_outlier_values <- boxplot_object$out
    outlier_values <- c(outlier_values, current_outlier_values)
    
    # get the outlier positions
    current_outlier_positions <- which(y %in% current_outlier_values)
    outlier_positions <- c(outlier_positions, current_outlier_positions)
    
    # replace outlier values with NA
    imputed_data[current_outlier_positions] <- NA
    
    # update iterator
    if (length(current_outlier_positions) > 0){
      k <- k + 1
    }
    
  }
  
  # deduce the number of iterations
  iterate <- k
  
  # impute outlier values
  if (method == "mean"){
    outlier_substitutes <- mean(imputed_data, na.rm = TRUE)
    imputed_data[outlier_positions] <- outlier_substitutes
  }
  else if (method == "median"){
    outlier_substitutes <- median(imputed_data, na.rm = TRUE)
    imputed_data[outlier_positions] <- outlier_substitutes
  }
  else if (method == "mode"){
    outlier_substitutes <- calculate_mode(x = na.omit(imputed_data),
                                          data_type = c("continuous", "discrete")[1])
    imputed_data[outlier_positions] <- outlier_substitutes
  }
  else if (method == "interpolate"){
    imputed_data <- as.numeric(forecast::na.interp(x = imputed_data))
    outlier_substitutes <- imputed_data[outlier_positions]
  }
  else{
    stop("Please enter appropriate information in the argument: method!")
  }
  
  # update the output object
  output[["outlier_values"]] <- outlier_values
  output[["outlier_positions"]] <- outlier_positions
  output[["outlier_substitutes"]] <- outlier_substitutes
  output[["iterate"]] <- iterate
  output[["method"]] <- method
  output[["coefficient_iqr"]] <- coefficient_iqr
  output[["imputed_data"]] <- imputed_data
  output[["raw_data"]] <- x
  
  output
}


# # example 1
# 
# x <- c(0.1, 0.2,6,5,5,6,7,8,8,9,9,9,10,10,25)
# 
# results <- impute_outliers(x = x,
#                            coefficient_iqr = 1.5,
#                            iterate = 10,
#                            method = c("interpolate", "mode", "median", "mean")[1])
# 
# results
# 
# boxplot(x[-results$outlier_positions])
# 
# boxplot(results$imputed_data)
# 
# 
# # example 2
# 
# # x <- rnorm(n = 500)
# x <- rexp(n = 500)
# 
# 
# boxplot(x)
# hist(x, probability = TRUE)
# lines(density(x))
# 
# results <- impute_outliers(x = x,
#                            coefficient_iqr = 1.5,
#                            iterate = 10,
#                            method = c("interpolate", "mode", "median", "mean")[1])
# 
# results
# 
# 
# boxplot(x[-results$outlier_positions])
# 
# boxplot(results$imputed_data)
# 
# hist(results$imputed_data, probability = TRUE)
# lines(density(results$imputed_data))
# 
# 
# results <- impute_outliers(x = x,
#                            coefficient_iqr = 1.5,
#                            iterate = 5,
#                            method = c("interpolate", "mode", "median", "mean")[1])
# 
# results
# 
# boxplot(x[-results$outlier_positions])
# 
# boxplot(results$imputed_data)
# 
# hist(results$imputed_data, probability = TRUE)
# lines(density(results$imputed_data))
