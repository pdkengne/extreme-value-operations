source("./src/get_knn.R")
source("./src/make_weights.R")
source("./src/initialize_ns_cluster_data.R")
source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/get_several_ns_gev_model_parameters.R")


calculate_ns_evd_cluster_attractors <- function(x, 
                                                covariates = NULL,
                                                cluster_models,
                                                minimum_cluster_size = 20,
                                                prior_cluster_weights = NULL,
                                                do.ci = FALSE,
                                                confidence_level = 0.95){
  # x:
  # covariates:
  # cluster_models:
  # minimum_cluster_size:
  # prior_cluster_weights:
  # do.ci:
  # confidence_level:
  
  nclusters <- length(cluster_models)
  
  if (is.null(covariates)){
    covariates <- lapply(cluster_models, function(model){
      model$cov.data
    })
    
    covariates <- do.call(what = rbind, covariates)
  }
  
  
  if (is.null(prior_cluster_weights)){
    initial_cluster_data <- initialize_ns_cluster_data(x = x, 
                                                       covariates = covariates,
                                                       nclusters = nclusters)
    
    cluster_data <- initial_cluster_data$cluster_data
    
    prior_cluster_sizes <- sapply(cluster_data, function(data){
      length(data)
    })
    
    prior_cluster_weights <-  make_weights(positives_values = prior_cluster_sizes)
  }
  
  cluster_models_parameters <- get_several_ns_gev_model_parameters(several_ns_gev_models = cluster_models, 
                                                                   data = covariates)
  
  cluster_attractors_matrix <- sapply(1:length(x), function(i){
    obs <- x[i]
    unnormalized_posterior <- sapply(1:nclusters, function(k){
      parameters <- cluster_models_parameters[[k]]
      
      location <- parameters$location[i]
      scale <- parameters$scale[i]
      shape <- parameters$shape[i]
      
      likelihood <- calculate_gev_pdf(x = obs,
                                      loc = location,
                                      scale = scale,
                                      shape = shape)

      likelihood*prior_cluster_weights[k]
    })
    unnormalized_posterior
  })
  
  cluster_attractors_matrix <- exp(cluster_attractors_matrix)
  
  if (class(cluster_attractors_matrix)[1] != "matrix"){
    cluster_attractors_matrix <- t(cluster_attractors_matrix)
  }
  
  mass <- apply(cluster_attractors_matrix, 2, sum)
  cluster_attractors_matrix <- cluster_attractors_matrix/mass
  
  cluster_attractors_frequencies_table <- apply(cluster_attractors_matrix, 2, which.max)
  
  cluster_attractors_frequencies <- sapply(1:nclusters, function(k){
    length(which(cluster_attractors_frequencies_table == k))
  })
  
  selected_cluster_id <- sort(which(cluster_attractors_frequencies >= minimum_cluster_size))
  
  nclusters <- length(selected_cluster_id)
  
  cluster_attractors_frequencies <- cluster_attractors_frequencies[selected_cluster_id]
  
  cluster_attractors_weights <- make_weights(positives_values = cluster_attractors_frequencies)
  
  # extract the selected gev models
  selected_cluster_models <- lapply(selected_cluster_id, function(k){
    model <- cluster_models[[k]]
    model
  })
  
  cluster_models_coefficients <- lapply(selected_cluster_id, function(k){
    model <- cluster_models[[k]]
    model$results$par
  })
  
  cluster_models_coefficients <- do.call(what = rbind, cluster_models_coefficients)
  
  
  if (do.ci){
    cluster_models_coefficients_ci <- lapply(selected_cluster_id, function(k){
      model <- cluster_models[[k]]
      extRemes::ci.fevd(x = model, 
                        type = "parameter",
                        alpha = 1 - confidence_level)
    })
  }
  else {
    cluster_models_coefficients_ci <- NULL
  }
  
  cluster_models_parameters <- get_several_ns_gev_model_parameters(several_ns_gev_models = selected_cluster_models, 
                                                                   data = covariates)
                                                                      
  # calculate the vector of pdf
  densities <- sapply(1:nrow(covariates), function(i){
    obs <- x[i]
    
    distributions <- sapply(1:length(cluster_attractors_weights), function(k){
      parameters <- cluster_models_parameters[[k]]
      
      coefficients <- c("location" = parameters$location[i], 
                        "scale" = parameters$scale[i], 
                        "shape" = parameters$shape[i])
      
      coefficients
    })
    
    pdf <- calculate_gev_mixture_model_pdf(x = obs,
                                           locations = distributions["location", ],
                                           scales = distributions["scale", ],
                                           shapes = distributions["shape", ],
                                           weights = cluster_attractors_weights,
                                           kind = c("geometric", "arithmetic", "harmonic")[2])
    
    pdf
  })
  
  
  densities <- densities[is.finite(densities)]
  
  loglik <- sum(log(densities))
  
  p <- nclusters
  q <- ncol(cluster_models_coefficients)
  n <- length(x)
  
  aic <- -2*loglik + 2*(q*p + p - 1)
  bic <- -2*loglik + log(n)*(q*p + p - 1)
  
  cluster_information_criteria <- c(aic, bic)
  names(cluster_information_criteria) <- c("AIC", "BIC")
  
  
  cluster_attractors_centers <- sapply(selected_cluster_id, function(k){
    mean(x[which(cluster_attractors_frequencies_table == k)])
  })
  
  cluster_data_list <- lapply(1:nclusters, function(k){
    positions <- which(cluster_attractors_frequencies_table == k)
    
    cluster_data <- x[positions]
    
    cluster_data
  })
  
  cluster_covariates_list <- lapply(1:nclusters, function(k){
    positions <- which(cluster_attractors_frequencies_table == k)
    
    predictors <- covariates %>% slice(positions)
    
    predictors
  })
  
  clusters <- as.numeric(cluster_attractors_frequencies_table)
  
  output <- list()
  
  # output[["cluster_attractors_matrix"]] <- cluster_attractors_matrix
  output[["clusters"]] <- clusters
  output[["cluster_attractors_frequencies"]] <- cluster_attractors_frequencies
  output[["cluster_attractors_weights"]] <- cluster_attractors_weights
  output[["cluster_attractors_centers"]] <- cluster_attractors_centers
  output[["cluster_data_list"]] <- cluster_data_list
  output[["cluster_covariates_list"]] <- cluster_covariates_list
  output[["cluster_models"]] <- selected_cluster_models
  output[["cluster_models_coefficients"]] <- cluster_models_coefficients
  output[["cluster_models_coefficients_ci"]] <- cluster_models_coefficients_ci
  output[["loglik"]] <- loglik
  output[["cluster_information_criteria"]] <- cluster_information_criteria
  output[["selected_cluster_id"]] <- selected_cluster_id
  
  output
}


# # example 1
# 
# source("./src/initialize_ns_cluster_data.R")
# source("./src/estimate_ns_evd_cluster_models.R")
# 
# n <- 1000
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(0.6, 1.3), sd = c(0.1, 0.1))
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- 1:n/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# covariates <- data.frame(trend = trend, random = rnd)
# 
# hist(x, nclass = NULL)
# 
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_ns_cluster_data(x = x,
#                                                    covariates = covariates,
#                                                    nclusters = nclusters)
# 
# names(initial_cluster_data)
# 
# # initial_cluster_data$cluster_data
# 
# # initial_cluster_data$cluster_covariates
# 
# 
# cluster_models <- estimate_ns_evd_cluster_models(input_cluster_data = initial_cluster_data,
#                                                  location.fun = ~ random,
#                                                  scale.fun = ~ 1,
#                                                  shape.fun = ~1)
# 
# cluster_models
# 
# 
# prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))
# 
# prior_cluster_weights
# 
# cluster_attractors <- calculate_ns_evd_cluster_attractors(x = x,
#                                                           covariates = covariates,
#                                                           cluster_models = cluster_models,
#                                                           prior_cluster_weights = prior_cluster_weights)
# 
# names(cluster_attractors)
# 
# cluster_attractors$cluster_attractors_centers
# 
# cluster_attractors$cluster_attractors_frequencies
# 
# cluster_attractors$cluster_attractors_weights
# 
# cluster_attractors$cluster_data_list
# 
# cluster_attractors$cluster_covariates_list

