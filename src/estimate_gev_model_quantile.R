# library(extRemes)

source("./src/estimate_gev_parameters.R")
source("./src/calculate_gev_inverse_cdf.R")


estimate_gev_model_quantile <- function(gev_model, 
                                        alpha,
                                        do.ci = TRUE,
                                        confidence_level = 0.95){
  # gev_model: an object associated with a result of the function "estimate_gev_parameters()"
  # alpha: order of the quantile to estimate
  # do.ci: boolean which indicates whether to return confidence interval or not
  # confidence_level: the desired confidence level for the estimated quantile
 
  if (alpha == 0 | alpha == 1){
    gev_parameters <- gev_model$results$par
    quantile <- calculate_gev_inverse_cdf(p = 1 - alpha, 
                                          loc = gev_parameters["location"], 
                                          scale = gev_parameters["scale"], 
                                          shape = gev_parameters["shape"])
    
    output <- data.frame("lower" = NA, "quantile" = NA, "upper" = NA)
    
    output[1, ] <- c(quantile, quantile, quantile)
  }
  else {
    quantile <- extRemes::return.level(x = gev_model, 
                                       return.period = 1/alpha, 
                                       alpha = 1 - confidence_level, 
                                       method = c("normal"), 
                                       do.ci = do.ci)
    
    output <- data.frame("lower" = NA, "quantile" = NA, "upper" = NA)
    
    output[1, ] <- quantile[names(quantile)]
  }
  
  as.matrix(output)
}



# # example 1
# 
# source("./src/generate_gev_sample.R")
# 
# x <- generate_gev_sample(n = 1000, loc = 1, scale = 0.5, shape = -0.2)
# 
# gev_model <- estimate_gev_parameters(x = x,
#                                      type = c("GEV", "Gumbel")[1],
#                                      method = c("MLE", "GMLE", "Lmoments")[1])
# 
# results <- estimate_gev_model_quantile(gev_model = gev_model,
#                                        alpha = 10^(-2),
#                                        do.ci = TRUE,
#                                        confidence_level = 0.95)
# 
# results
