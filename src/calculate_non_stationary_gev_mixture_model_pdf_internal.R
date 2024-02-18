# library(dplyr)

source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/get_several_ns_gev_model_normalized_parameters.R")


calculate_non_stationary_gev_mixture_model_pdf_internal <- function(selected_full_ns_gev_models,
                                                                    weights,
                                                                    partial_data,
                                                                    partial_data_covariates,
                                                                    use_extremal_index){
  # selected_full_ns_gev_models: 
  # weights: 
  # partial_data: 
  # partial_data_covariates: 
  # use_extremal_index: 

  # calculate the normalized gev parameters
  normalized_gev_parameters <- get_several_ns_gev_model_normalized_parameters(several_ns_gev_models = selected_full_ns_gev_models,
                                                                              data = partial_data_covariates,
                                                                              use_extremal_index = use_extremal_index,
                                                                              normalize_parameters = TRUE)
  
  # calculate the vector of pdf
  mixture_distributions <- sapply(1:nrow(partial_data_covariates), function(i){
    obs <- partial_data[i]
    
    distributions <- sapply(1:length(weights), function(k){
      parameters <- normalized_gev_parameters[[k]]
      
      coefficients <- c("location" = parameters$location[i], 
                        "scale" = parameters$scale[i], 
                        "shape" = parameters$shape[i])
      
      coefficients
    })
    
    pdf <- calculate_gev_mixture_model_pdf(x = obs,
                                           locations = distributions["location", ],
                                           scales = distributions["scale", ],
                                           shapes = distributions["shape", ],
                                           weights = weights,
                                           kind = c("geometric", "arithmetic", "harmonic")[2])
    
    pdf
  })
  
  mixture_distributions
}



# densities <- calculate_non_stationary_gev_mixture_model_pdf_internal(selected_full_ns_gev_models = selected_full_ns_gev_models,
#                                                                      weights = weights,
#                                                                      partial_data = partial_data,
#                                                                      partial_data_covariates = partial_data_covariates,
#                                                                      use_extremal_index = use_extremal_index)
