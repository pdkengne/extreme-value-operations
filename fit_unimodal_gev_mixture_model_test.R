# library(extRemes) 

source("./src/calculate_modes.R")


fit_unimodal_gev_mixture_model <- function(x, 
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
  
  # create an empty output object
  output <- list()
  
  n <- length(x)
  p <- nb_gev_models
  
  z <- as.numeric(ggplot2::cut_number(x = x, n = p))
  
  clusters_freq <- table(z)
  clusters_labels <- as.numeric(names(clusters_freq))
  omega <- as.numeric(prop.table(clusters_freq))
  
  while (min(as.numeric(clusters_freq)) < min_cluster_size & p > 1){
    p <- p - 1
    
    z <- as.numeric(ggplot2::cut_number(x = x, n = p))
    
    clusters_freq <- table(z)
    clusters_labels <- as.numeric(names(clusters_freq))
    omega <- as.numeric(prop.table(clusters_freq))
  }
  
  theta <- sapply(clusters_labels, function(k){
    if (k < p){
      if (k > 1){
        y <- c(tail(sort(x[z == k - 1]), n  = left_cluster_extension_size),
               x[z == k], 
               head(sort(x[z == k + 1]), n  = right_cluster_extension_size))
      } else{
        y <- c(x[z == k], head(sort(x[z == k + 1]), n  = right_cluster_extension_size))
      }
    } else{
      y <- c(tail(sort(x[z == k - 1]), n  = left_cluster_extension_size), x[z == k])
    }
    
    y <- extRemes::revd(n = 1000)
    model <- extRemes::fevd(x = y, type = "GEV")
    res <- summary(model, silent = TRUE)
    
    parameters <- c(model$results$par, res$nllh)
    names(parameters) <- c("location", "scale", "shape", "nllh")
    parameters
  })
  
  locations <- theta["location", ]
  scales <- theta["scale", ]
  shapes <- theta["shape", ]
  
  posterior <- sapply(1:n, function(i){
    obs <- x[i]
    likelihood <- sapply(clusters_labels, function(k){
      location <- locations[k]
      scale <- scales[k]
      shape <- shapes[k]
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
  
  #--------------------
  z <- apply(posterior, 2, which.max)
  clusters_freq <- table(z)
  clusters_labels <- as.numeric(names(clusters_freq))
  omega <- as.numeric(prop.table(clusters_freq))
  #--------------------
  
  current_negative_loglik <- sum(theta["nllh", ])
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
      
      z <- as.numeric(ggplot2::cut_number(x = x, n = p))
      
      clusters_freq <- table(z)
      clusters_labels <- as.numeric(names(clusters_freq))
      omega <- as.numeric(prop.table(clusters_freq))
    }
    
    theta <- sapply(clusters_labels, function(k){
      if (k < p){
        if (k > 1){
          y <- c(tail(sort(x[z == k - 1]), n  = left_cluster_extension_size),
                 x[z == k], 
                 head(sort(x[z == k + 1]), n  = right_cluster_extension_size))
        } else{
          y <- c(x[z == k], head(sort(x[z == k + 1]), n  = right_cluster_extension_size))
        }
      } else{
        y <- c(tail(sort(x[z == k - 1]), n  = left_cluster_extension_size), x[z == k])
      }
      
      model <- extRemes::fevd(x = y, type = "GEV")
      res <- summary(model, silent = TRUE)
      
      parameters <- c(model$results$par, res$nllh)
      names(parameters) <- c("location", "scale", "shape", "nllh")
      parameters
    })
    
    locations <- theta["location", ]
    scales <- theta["scale", ]
    shapes <- theta["shape", ]
    
    posterior <- sapply(1:n, function(i){
      obs <- x[i]
      likelihood <- sapply(clusters_labels, function(k){
        location <- locations[k]
        scale <- scales[k]
        shape <- shapes[k]
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
    
    current_tolerance <- abs(current_negative_loglik - sum(theta["nllh", ]))
    current_negative_loglik <- sum(theta["nllh", ])
    
    print(paste0("Iteration: ", current_iteration, ", Tolerance: ", current_tolerance))
  }
  
  final_models <- lapply(clusters_labels, function(k){
    
    if (k < p){
      if (k > 1){
        y <- c(tail(sort(x[z == k - 1]), n  = left_cluster_extension_size),
               x[z == k], 
               head(sort(x[z == k + 1]), n  = right_cluster_extension_size))
      } else{
        y <- c(x[z == k], head(sort(x[z == k + 1]), n  = right_cluster_extension_size))
      }
    } else{
      y <- c(tail(sort(x[z == k - 1]), n  = left_cluster_extension_size), x[z == k])
    }
    
    model <- extRemes::fevd(x = y, type = "GEV")
    model
  })
  
  parameters <- sapply(final_models, function(model){
    res <- summary(model, silent = TRUE)
    model$results$par
  })
  
  nllh <- sapply(final_models, function(model){
    res <- summary(model, silent = TRUE)
    res$nllh
  })
  
  names(nllh) <- clusters_labels
  
  p <- length(clusters_labels)
  
  aic <- 2*sum(nllh) + 2*3*p
  bic <- 2*sum(nllh) + log(n)*3*p
  information_criterions <- c(aic, bic)
  names(information_criterions) <- c("AIC", "BIC")
  
  cluster_sizes <- as.numeric(table(z))
  names(cluster_sizes) <- clusters_labels
  
  # update the output object
  output[["last_iteration"]] <- current_iteration
  output[["last_tolerance"]] <- current_tolerance
  output[["nclusters"]] <- p
  output[["cluster_sizes"]] <- cluster_sizes
  output[["cluster_weights"]] <- omega
  output[["cluster_negative_loglikelihoods"]] <- nllh
  output[["information_criterions"]] <- information_criterions
  output[["cluster_gev_model_parameters"]] <- parameters
  output[["clusters"]] <- z
  output[["data"]] <- x
  output[["cluster_models"]] <- final_models
  
  output
}


# example 1

source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_gev_mixture_model.R")

# x <- rnorm(n = 2000)

x <- extRemes::revd(n = 2000)

modes_object <- calculate_modes(x = x)

plot_modes(modes_object)

p <- 5

results <- fit_unimodal_gev_mixture_model(x = x,
                                          nb_gev_models = p,
                                          min_cluster_size = 20,
                                          max_iteration = 1,
                                          left_cluster_extension_size = 20,
                                          right_cluster_extension_size = 20,
                                          tolerance = 10^(-3))

names(results)

# [1] "last_iteration"                  "last_tolerance"                  "nclusters"                       "cluster_sizes"                   "cluster_weights"
# [6] "cluster_negative_loglikelihoods" "information_criterions"          "cluster_gev_model_parameters"    "clusters"                        "data"
# [11] "cluster_models"


results$nclusters

results$cluster_sizes


results$information_criterions

results$cluster_gev_model_parameters

results$cluster_weights

#results$cluster_models


plot_fit_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")



# example 2

source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_gev_mixture_model.R")


x <- bmixture::rmixnorm(n = 3000, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))

modes_object <- calculate_modes(x = x)

plot_modes(modes_object)


p <- 3

results <- fit_unimodal_gev_mixture_model(x = x,
                                          nb_gev_models = p,
                                          min_cluster_size = 20,
                                          max_iteration = 40,
                                          left_cluster_extension_size = 60,
                                          right_cluster_extension_size = 10,
                                          tolerance = 10^(-3))

names(results)

results$nclusters

results$cluster_sizes

results$cluster_weights

results$information_criterions

results$cluster_gev_model_parameters

results$cluster_models


plot_fit_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")


# example 3


source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_gev_mixture_model.R")

data(faithful, package = "datasets")

data <- faithful

data$scaled_waiting <- scale(data$waiting)

names(data)

x <- data$eruptions

modes_object <- calculate_modes(x = x)

plot_modes(modes_object)

p <- 2

z <- x[x > 3]

results <- fit_unimodal_gev_mixture_model(x = z,
                                          nb_gev_models = p,
                                          min_cluster_size = 20,
                                          max_iteration = 40,
                                          left_cluster_extension_size = 5,
                                          right_cluster_extension_size = 10,
                                          tolerance = 10^(-3))

names(results)

results$nclusters

results$cluster_sizes

results$cluster_weights

results$information_criterions

results$cluster_gev_model_parameters

results$cluster_models


plot_fit_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")

abline(h = 0, lty = "dotted")


# example 4

source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_gev_mixture_model.R")

x <- rexp(n = 10000)


modes_object <- calculate_modes(x = x)

plot_modes(modes_object)


p <- 10

results <- fit_unimodal_gev_mixture_model(x = x,
                                          nb_gev_models = p,
                                          min_cluster_size = 20,
                                          max_iteration = 40,
                                          left_cluster_extension_size = 10,
                                          right_cluster_extension_size = 10,
                                          tolerance = 10^(-3))

names(results)

# [1] "last_iteration"                  "last_tolerance"                  "nclusters"                       "cluster_sizes"                   "cluster_weights"
# [6] "cluster_negative_loglikelihoods" "information_criterions"          "cluster_gev_model_parameters"    "clusters"                        "data"
# [11] "cluster_models"


results$nclusters

results$cluster_sizes

results$information_criterions

results$cluster_gev_model_parameters

results$cluster_models


plot_fit_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")



