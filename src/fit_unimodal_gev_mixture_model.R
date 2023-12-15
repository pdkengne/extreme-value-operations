# library(extRemes) 
# library(fitdistrplus)

source("./src/extract_nlargest_sample.R")
source("./src/get_candidate_block_sizes.R")
source("./src/estimate_several_gev_models.R")
source("./src/estimate_several_standardized_block_maxima_mean.R")

source("./src/estimate_gev_mixture_model_automatic_weights.R")


source("./src/extract_peaks_over_threshold.R")
source("./src/find_threshold_associated_with_given_block_size.R")

source("./src/estimate_extremal_index.R")
source("./src/calculate_power_gev_parameters.R")
source("./src/extract_block_maxima.R")
source("./src/extract_block_maxima_with_indexes.R")


fit_unimodal_gev_mixture_model <- function(x, 
                                           block_sizes = NULL,
                                           minimum_nblocks = 50,
                                           threshold = NULL,
                                           confidence_level = 0.95,
                                           use_extremal_index = TRUE,
                                           prior = c("identic", "pessimistic"),
                                           method = c("MLE", "GMLE", "Lmoments")[1]){
  # x: vector of observations
  # nb_gev_models: a positive integer which indicates the number of gev models to start with
  # min_cluster_size: indicates the minimum number of elements in a cluster
  # max_iteration: indicates the maximumlibrary(fitdistrplus) number of iterations to perform in the CEM algorithm
  # tolerance: indicates the threshold for the difference between two consecutive negative log likelihoods
  # right_cluster_extension_size & left_cluster_extension_size: indicates the number of nearest observations
  #                                                               from the surrounding clusters to includes
  
  # create an empty output object
  output <- list()
  
  n <- length(x)
  
  p <- length(block_sizes)
  
  omega <- rep(x = 1, times = p)/p
  
  theta <- sapply(1:p, function(k){
    y <-  extract_block_maxima(x, block_size = block_sizes[k])
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
    likelihood <- sapply(1:p, function(k){
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
  
  selected_block_sizes <- block_sizes[clusters_labels]
  
  final_models <- lapply(clusters_labels, function(k){
    y <-  extract_block_maxima(x, block_size = block_sizes[k])
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
  output[["nclusters"]] <- length(cluster_sizes)
  output[["cluster_sizes"]] <- cluster_sizes
  output[["selected_block_sizes"]] <- selected_block_sizes
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
source("./src/plot_several_standardized_block_maxima_mean.R")

#x <- rnorm(n = 1000)

n <- 3000

loc <- 0
scale <- 1
shape <- 0.01

x <- extRemes::revd(n = n, loc = loc, scale = scale, shape = shape)

dens <- extRemes::devd(x = sort(x), loc = loc, scale = scale, shape = shape)

modes_object <- calculate_modes(x = x)

plot_modes(modes_object)

results <- fit_unimodal_gev_mixture_model(x = x, block_sizes = c(1:50))

names(results)

# [1] "last_iteration"                  "last_tolerance"                  "nclusters"                       "cluster_sizes"                   "cluster_weights"
# [6] "cluster_negative_loglikelihoods" "information_criterions"          "cluster_gev_model_parameters"    "clusters"                        "data"
# [11] "cluster_models"


results$nclusters

results$cluster_sizes

results$selected_block_sizes

results$information_criterions

results$cluster_gev_model_parameters

results$cluster_weights

#results$cluster_models


plot_fit_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")


lines(sort(x), dens, col = 4, lwd = 2)


# example 2

source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_gev_mixture_model.R")
source("./src/plot_several_standardized_block_maxima_mean.R")


x <- bmixture::rmixnorm(n = 3000, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))

modes_object <- calculate_modes(x = x)

plot_modes(modes_object)


results <- fit_unimodal_gev_mixture_model(x = x, block_sizes = c(1:50))

names(results)

results$nclusters

results$cluster_sizes

results$selected_block_sizes

results$information_criterions

results$cluster_gev_model_parameters

results$cluster_weights

#results$cluster_models


plot_fit_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")




plot_several_standardized_block_maxima_mean(x = x,
                                            block_sizes = results$selected_block_sizes,
                                            confidence_level = 0.95,
                                            equivalent = FALSE,
                                            method = c("MLE", "GMLE", "Lmoments")[1])


plot_several_standardized_block_maxima_mean(x = x,
                                            block_sizes = results$selected_block_sizes,
                                            confidence_level = 0.95,
                                            equivalent = TRUE,
                                            method = c("MLE", "GMLE", "Lmoments")[1])


selection <- estimate_several_standardized_block_maxima_mean(x = x, 
                                                             block_sizes = results$selected_block_sizes, 
                                                             confidence_level = 0.95, 
                                                             method = c("MLE", "GMLE", "Lmoments")[1])

selection

blocks <- as.numeric(rownames(selection$selected))
blocks


final_results <- fit_unimodal_gev_mixture_model(x = x, block_sizes = blocks)

#names(final_results)

final_results$nclusters

final_results$cluster_sizes

final_results$selected_block_sizes

final_results$information_criterions

final_results$cluster_gev_model_parameters

final_results$cluster_weights

#final_results$cluster_models


plot_fit_gev_mixture_model(gev_mixture_model_object = final_results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")


lines(sort(x), dens, col = 4, lwd = 2)



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



