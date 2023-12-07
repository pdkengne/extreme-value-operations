#library(extRemes)
#library(tidyverse)


library(dplyr)

source("./src/calculate_modes.R")


# use the function: extRemes::findpars(x, use.blocks = FALSE, ..., qcov = NULL)

  
fit_ns_gev_mixture_model <- function(x, 
                                     data,
                                     location.fun = ~ 1,
                                     scale.fun = ~ 1,
                                     shape.fun = ~ 1,
                                     use.phi = FALSE,
                                     nb_gev_models = 2, 
                                     min_cluster_size = 20, 
                                     max_iteration = 50,
                                     tolerance = 10^(-3),
                                     left_cluster_extension_size = 20,
                                     right_cluster_extension_size = 20){
  # x: vector of observations
  # nb_gev_models: a positive integer which indicates the number of gev models to start with
  # min_cluster_size: indicates the minimum number of elements in a cluster
  # max_iteration: indicates the maximum number of iterations to perform in the CEM algorithm
  # tolerance: indicates the threshold for the difference between two consecutive negative log likelihoods
  # right_cluster_extension_size & left_cluster_extension_size: indicates the number of nearest observations
  #                                                               from the surrounding clusters to includes
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # data: dataframe of covariates for linear modeling of the gev model parameters
  
  # create an empty output object
  output <- list()
  
  n <- length(x)
  p <- nb_gev_models
  
  if (p > 1){
    modes_object <- calculate_modes(x = x)
    density_minima <- modes_object$density_minima 
    density_minima_selected <- tail(sort(density_minima), n = p - 1)
    breaks = modes_object$density_minima_argument
    breaks <- c(min(x), breaks[density_minima %in% density_minima_selected], max(x))
    z <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
  } else{
    z <- rep(x = 1, times = n)
  }
  
  clusters_freq <- table(z)
  clusters_labels <- as.numeric(names(clusters_freq))
  omega <- as.numeric(prop.table(clusters_freq))
  
  while (min(as.numeric(clusters_freq)) < min_cluster_size & p > 1){
    p <- p - 1
    
    modes_object <- calculate_modes(x = x)
    density_minima <- modes_object$density_minima 
    density_minima_selected <- tail(sort(density_minima), n = p - 1)
    breaks = modes_object$density_minima_argument
    breaks <- c(min(x), breaks[density_minima %in% density_minima_selected], max(x))
    z <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
    
    clusters_freq <- table(z)
    clusters_labels <- as.numeric(names(clusters_freq))
    omega <- as.numeric(prop.table(clusters_freq))
  }
  
  theta <- lapply(clusters_labels, function(k){
    if (k < p){
      if (k > 1){
        y <- c(tail(sort(x[z == k - 1]), n = left_cluster_extension_size),
               x[z == k], 
               head(sort(x[z == k + 1]), n = right_cluster_extension_size))
        
        idx_1 <- which(z == k)
        df_1 <- data %>% slice(idx_1)
        
        idx_2 <- tail(order(x[z == k - 1]), n = left_cluster_extension_size)
        df_2 <- data %>% slice(which(z == k - 1)) %>% slice(idx_2)
        
        idx_3 <- head(order(x[z == k + 1]), n = right_cluster_extension_size)
        df_3 <- data %>% slice(which(z == k + 1)) %>% slice(idx_3)
        
        df <- data.frame(rbind(df_2, df_1, df_3))
      } else{
        y <- c(tail(sort(x[z == k - 1]), n = left_cluster_extension_size), x[z == k])
        
        idx_1 <- which(z == k)
        df_1 <- data %>% slice(idx_1)
        
        idx_2 <- tail(order(x[z == k - 1]), n = left_cluster_extension_size)
        df_2 <- data %>% slice(which(z == k - 1)) %>% slice(idx_2)
        
        df <- data.frame(rbind(df_2, df_1))
      }
    } else{
      y <- c(tail(sort(x[z == k - 1]), n = left_cluster_extension_size), x[z == k])
      
      idx_1 <- which(z == k)
      df_1 <- data %>% slice(idx_1)
      
      idx_2 <- tail(order(x[z == k - 1]), n = left_cluster_extension_size)
      df_2 <- data %>% slice(which(z == k - 1)) %>% slice(idx_2)
      
      df <- data.frame(rbind(df_2, df_1))
    }
    
    model <- extRemes::fevd(x = y, 
                            data = df, 
                            location.fun = location.fun,
                            scale.fun = scale.fun, 
                            shape.fun = shape.fun, 
                            use.phi = use.phi,
                            type = "GEV",
                            method = "MLE")
    
    res <- summary(model, silent = TRUE)
    par <- extRemes::findpars(model)
    
    parameters <- append(par, res$nllh)
    names(parameters) <- c("location", "scale", "shape", "nllh")
    parameters
  })
  
  posterior <- sapply(1:n, function(i){
    obs <- x[i]
    likelihood <- sapply(clusters_labels, function(k){
      parameters_list <- theta[[k]]
      
      locations <- parameters_list$location
      scales <- parameters_list$scale
      shapes <- parameters_list$shape
      
      location <- locations[i]
      scale <- scales[i]
      shape <- shapes[i]
      
      dens <- extRemes::devd(x = obs, 
                             loc = location, 
                             scale = scale, 
                             shape = shape, 
                             log = FALSE, 
                             type = "GEV")
      
      prior <- omega[k]
      prior*dens
    })
    likelihood/sum(likelihood)
  })
  
  posterior_nb_na <- sum(is.na(posterior))
  
  if (posterior_nb_na != 0){
    stop("Sorry, algorithm does not converge with the current inputs !")
  }
  
  negative_loglik <- sapply(clusters_labels, function(k){
    parameters_list <- theta[[k]]
    parameters_list$nllh
  })
  
  current_negative_loglik <- sum(negative_loglik)
  current_tolerance <- tolerance + 1
  current_iteration <- 1
  
  while (current_iteration < max_iteration & current_tolerance > tolerance){
    current_iteration <- current_iteration + 1
    
    if (class(posterior)[1] == "numeric"){
      z <- as.numeric(posterior)
    } else{
      z <- apply(posterior, 2, which.max)
    }
    
    clusters_freq <- table(z)
    clusters_labels <- as.numeric(names(clusters_freq))
    omega <- as.numeric(prop.table(clusters_freq))
    p <- length(clusters_labels)
    
    while (min(as.numeric(clusters_freq)) < min_cluster_size & p > 1){
      p <- p - 1
      
      modes_object <- calculate_modes(x = x)
      density_minima <- modes_object$density_minima 
      density_minima_selected <- tail(sort(density_minima), n = p - 1)
      breaks = modes_object$density_minima_argument
      breaks <- c(min(x), breaks[density_minima %in% density_minima_selected], max(x))
      z <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
      
      clusters_freq <- table(z)
      clusters_labels <- as.numeric(names(clusters_freq))
      omega <- as.numeric(prop.table(clusters_freq))
    }
    
    theta <- lapply(clusters_labels, function(k){
      if (k < p){
        if (k > 1){
          y <- c(tail(sort(x[z == k - 1]), n = left_cluster_extension_size),
                 x[z == k], 
                 head(sort(x[z == k + 1]), n = right_cluster_extension_size))
          
          idx_1 <- which(z == k)
          df_1 <- data %>% slice(idx_1)
          
          idx_2 <- tail(order(x[z == k - 1]), n = left_cluster_extension_size)
          df_2 <- data %>% slice(which(z == k - 1)) %>% slice(idx_2)
          
          idx_3 <- head(order(x[z == k + 1]), n = right_cluster_extension_size)
          df_3 <- data %>% slice(which(z == k + 1)) %>% slice(idx_3)
          
          df <- data.frame(rbind(df_2, df_1, df_3))
        } else{
          y <- c(x[z == k], head(sort(x[z == k + 1]), n = right_cluster_extension_size))
          
          idx_1 <- which(z == k)
          df_1 <- data %>% slice(idx_1)
          
          idx_2 <- head(order(x[z == k + 1]), n = right_cluster_extension_size)
          df_2 <- data %>% slice(which(z == k + 1)) %>% slice(idx_2)
          
          df <- data.frame(rbind(df_1, df_2))
        }
      } else{
        y <- c(tail(sort(x[z == k - 1]), n = left_cluster_extension_size), x[z == k])
        
        idx_1 <- which(z == k)
        df_1 <- data %>% slice(idx_1)
        
        idx_2 <- tail(order(x[z == k - 1]), n = left_cluster_extension_size)
        df_2 <- data %>% slice(which(z == k - 1)) %>% slice(idx_2)
        
        df <- data.frame(rbind(df_2, df_1))
      }
      
      model <- extRemes::fevd(x = y, 
                              data = df, 
                              location.fun = location.fun,
                              scale.fun = scale.fun, 
                              shape.fun = shape.fun, 
                              use.phi = use.phi,
                              type = "GEV",
                              method = "MLE")
      
      res <- summary(model, silent = TRUE)
      par <- extRemes::findpars(model)
      
      parameters <- append(par, res$nllh)
      names(parameters) <- c("location", "scale", "shape", "nllh")
      parameters
    })
    
    posterior <- sapply(1:n, function(i){
      obs <- x[i]
      likelihood <- sapply(clusters_labels, function(k){
        parameters_list <- theta[[k]]
        
        locations <- parameters_list$location
        scales <- parameters_list$scale
        shapes <- parameters_list$shape
        
        location <- locations[i]
        scale <- scales[i]
        shape <- shapes[i]
        
        dens <- extRemes::devd(x = obs, 
                               loc = location, 
                               scale = scale, 
                               shape = shape, 
                               log = FALSE, 
                               type = "GEV")
        
        prior <- omega[k]
        prior*dens
      })
      likelihood/sum(likelihood)
    })
    
    posterior_nb_na <- sum(is.na(posterior))
    
    if (posterior_nb_na != 0){
      stop("Sorry, algorithm does not converge with the current inputs !")
    }
    
    negative_loglik <- sapply(clusters_labels, function(k){
      parameters_list <- theta[[k]]
      parameters_list$nllh
    })
    
    current_tolerance <- abs(current_negative_loglik - sum(negative_loglik))
    current_negative_loglik <- sum(negative_loglik)
    
    print(paste0("Iteration: ", current_iteration, ", Tolerance: ", current_tolerance))
  }
  
  final_models <- lapply(clusters_labels, function(k){
    if (k < p){
      if (k > 1){
        y <- c(tail(sort(x[z == k - 1]), n = left_cluster_extension_size),
               x[z == k], 
               head(sort(x[z == k + 1]), n = right_cluster_extension_size))
        
        idx_1 <- which(z == k)
        df_1 <- data %>% slice(idx_1)
        
        idx_2 <- tail(order(x[z == k - 1]), n = left_cluster_extension_size)
        df_2 <- data %>% slice(which(z == k - 1)) %>% slice(idx_2)
        
        idx_3 <- head(order(x[z == k + 1]), n = right_cluster_extension_size)
        df_3 <- data %>% slice(which(z == k + 1)) %>% slice(idx_3)
        
        df <- data.frame(rbind(df_2, df_1, df_3))
      } else{
        y <- c(x[z == k], head(sort(x[z == k + 1]), n = right_cluster_extension_size))
        
        idx_1 <- which(z == k)
        df_1 <- data %>% slice(idx_1)
        
        idx_2 <- head(order(x[z == k + 1]), n = right_cluster_extension_size)
        df_2 <- data %>% slice(which(z == k + 1)) %>% slice(idx_2)
        
        df <- data.frame(rbind(df_1, df_2))
      }
    } else{
      y <- c(tail(sort(x[z == k - 1]), n = left_cluster_extension_size), x[z == k])
      
      idx_1 <- which(z == k)
      df_1 <- data %>% slice(idx_1)
      
      idx_2 <- tail(order(x[z == k - 1]), n = left_cluster_extension_size)
      df_2 <- data %>% slice(which(z == k - 1)) %>% slice(idx_2)
      
      df <- data.frame(rbind(df_2, df_1))
    }
    
    model <- extRemes::fevd(x = y, 
                            data = df, 
                            location.fun = location.fun,
                            scale.fun = scale.fun, 
                            shape.fun = shape.fun, 
                            use.phi = use.phi,
                            type = "GEV",
                            method = "MLE")
    
    model
  })
  
  model_residuals <- lapply(final_models, function(model){
    residuals <- extRemes::trans(model)
    residuals
  })
  
  model_residuals_fit <- lapply(model_residuals, function(residuals){
    model <- extRemes::fevd(x = residuals, 
                            type = "GEV", 
                            method = "MLE")
    model
  })
  
  model_residuals_diagnosics <- lapply(model_residuals_fit, function(model){
    confidence_intervals <- extRemes::ci.fevd(x = model, 
                                              alpha = 0.05, 
                                              type = "parameter")
    confidence_intervals
  })
  
  model_parameters <- lapply(final_models, function(model){
    pars <- extRemes::findpars(model)
    pars
  })
  
  model_observations <- lapply(final_models, function(model){
    model$x
  })
  
  model_covariates <- lapply(final_models, function(model){
    model$cov.data
  })
  
  model_coefficients <- sapply(final_models, function(model){
    res <- summary(model, silent = TRUE)
    model$results$par
  })
  
  nllh <- sapply(final_models, function(model){
    res <- summary(model, silent = TRUE)
    res$nllh
  })
  
  names(nllh) <- clusters_labels
  
  p <- length(clusters_labels)
  q <- nrow(model_coefficients)
  
  aic <- 2*sum(nllh) + 2*q*p
  bic <- 2*sum(nllh) + log(n)*q*p
  information_criterions <- c(aic, bic)
  names(information_criterions) <- c("AIC", "BIC")
  
  cluster_sizes <- as.numeric(table(z))
  names(cluster_sizes) <- clusters_labels
  
  # update the output object
  output[["last_iteration"]] <- current_iteration
  output[["last_tolerance"]] <- current_tolerance
  output[["left_cluster_extension_size"]] <- left_cluster_extension_size
  output[["right_cluster_extension_size"]] <- right_cluster_extension_size
  output[["nclusters"]] <- p
  output[["cluster_sizes"]] <- cluster_sizes
  output[["cluster_weights"]] <- omega
  output[["cluster_negative_loglikelihoods"]] <- nllh
  output[["information_criterions"]] <- information_criterions
  output[["cluster_gev_model_coefficients"]] <- model_coefficients
  output[["cluster_gev_model_parameters"]] <- model_parameters
  output[["cluster_gev_model_observations"]] <- model_observations
  output[["cluster_gev_model_covariates"]] <- model_covariates
  output[["clusters"]] <- z
  output[["data"]] <- x
  output[["covariates"]] <- data
  output[["cluster_residuals_data"]] <- model_residuals
  output[["cluster_residuals_models"]] <- model_residuals_fit
  output[["cluster_residuals_diagnostics"]] <- model_residuals_diagnosics
  output[["cluster_models"]] <- final_models
  
  output
}



# # example 1
# 
# source("./src/calculate_modes.R")
# source("./src/plot_modes.R")
# 
# #library(extRemes)
# 
# data("PORTw", package = "extRemes")
# 
# data <- PORTw
# 
# x <- PORTw$TMX1
# 
# modes_object <- calculate_modes(x = x)
# 
# plot_modes(modes_object)
# 
# p <- 2
# 
# results <- fit_ns_gev_mixture_model(x = x,
#                                     data = data,
#                                     location.fun = ~ AOindex,
#                                     scale.fun = ~ 1,
#                                     shape.fun = ~ 1,
#                                     use.phi = FALSE,
#                                     nb_gev_models = p,
#                                     min_cluster_size = 20,
#                                     max_iteration = 50,
#                                     tolerance = 10^(-3),
#                                     left_cluster_extension_size = 20,
#                                     right_cluster_extension_size = 20)
# 
# names(results)
# 
# # [1] "last_iteration"                  "last_tolerance"                  "left_cluster_extension_size"    
# # [4] "right_cluster_extension_size"    "nclusters"                       "cluster_sizes"                  
# # [7] "cluster_weights"                 "cluster_negative_loglikelihoods" "information_criterions"         
# # [10] "cluster_gev_model_coefficients"  "cluster_gev_model_parameters"    "cluster_gev_model_observations" 
# # [13] "cluster_gev_model_covariates"    "clusters"                        "data"                           
# # [16] "covariates"                      "cluster_residuals_data"          "cluster_residuals_models"       
# # [19] "cluster_residuals_diagnostics"   "cluster_models" 
# 
# 
# results$cluster_gev_model_coefficients
# 
# results$cluster_residuals_diagnostics
# 
# 
# # example 2
# 
# source("./src/calculate_modes.R")
# source("./src/plot_modes.R")
# 
# library(Hmisc)
# 
# data(faithful)
# 
# data <- faithful
# 
# data$scaled_waiting <- scale(data$waiting)
# 
# names(data)
# 
# x <- data$eruptions
# 
# modes_object <- calculate_modes(x = x)
# 
# plot_modes(modes_object)
# 
# p <- 2
# 
# results <- fit_ns_gev_mixture_model(x = x,
#                                     data = data,
#                                     location.fun = ~ scaled_waiting,
#                                     scale.fun = ~ scaled_waiting,
#                                     shape.fun = ~ 1,
#                                     use.phi = FALSE,
#                                     nb_gev_models = p,
#                                     min_cluster_size = 20,
#                                     max_iteration = 50,
#                                     tolerance = 10^(-3),
#                                     left_cluster_extension_size = 5,
#                                     right_cluster_extension_size = 5)
# 
# names(results)
# 
# # [1] "last_iteration"                  "last_tolerance"                  "left_cluster_extension_size"    
# # [4] "right_cluster_extension_size"    "nclusters"                       "cluster_sizes"                  
# # [7] "cluster_weights"                 "cluster_negative_loglikelihoods" "information_criterions"         
# # [10] "cluster_gev_model_coefficients"  "cluster_gev_model_parameters"    "cluster_gev_model_observations" 
# # [13] "cluster_gev_model_covariates"    "clusters"                        "data"                           
# # [16] "covariates"                      "cluster_residuals_data"          "cluster_residuals_models"       
# # [19] "cluster_residuals_diagnostics"   "cluster_models" 
# 
# 
# results$cluster_gev_model_coefficients
# 
# results$cluster_gev_model_parameters


