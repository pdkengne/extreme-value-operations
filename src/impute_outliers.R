# library(forecast)

source("./src/calculate_mode.R")

impute_outliers <- function(x, 
                            coefficient_iqr = 1.5, 
                            method = c("interpolate", "mode", "median", "mean")[1]){
  # x: vector of observations
  # method: indicates the method of outlier values imputation
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
#                            method = c("interpolate", "mode", "median", "mean")[1])
# 
# results
# 
# 
# results <- impute_outliers(x = x,
#                            coefficient_iqr = 3,
#                            method = "linear")
# 
# results

