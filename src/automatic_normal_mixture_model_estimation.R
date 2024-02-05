setwd("/home/pdkengne/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

# library(bmixture)
# library(factoextra)
# library(zoo)
# library(FactoMineR)

library(ggplot2)
library(fitdistrplus)
library(dplyr)
library(dbscan)
library(bmixture)


source("./src/get_knn.R")
source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/make_weights.R")
source("./src/initialize_cluster_data.R")
source("./src/estimate_normal_cluster_models.R")
source("./src/calculate_normal_cluster_attractors.R")
source("./src/calculate_normal_mixture_model_cdf.R")
source("./src/calculate_normal_mixture_model_pdf.R")
source("./src/calculate_normal_mixture_model_inverse_cdf.R")


# generate data from a Normal mixture model
library(mixR)
set.seed(102)
x = rmixnormal(1000, c(0.3, 0.7), c(-2, 3), c(2, 1))

mod1 = mixfit(x, ncomp = 2) 
mod1

library(mixtools)
data(faithful)

x <- faithful$waiting

mod1 = mixfit(x, ncomp = 2) 
mod1

x <- faithful$eruptions

x <- rnorm(n = 1000)

x <- bmixture::rmixnorm(n = 1000, weight = c(2/4, 1/4, 1/4), mean = c(-2, +2, +10), sd = c(1, 1, 1))


n <- length(x)


modes_object <- calculate_modes(x = x)

plot_modes(modes_object)

centers <- modes_object$density_maxima_argument
centers

# centers <- c(-1, +1, +3)

nclusters <- length(centers)

# nclusters <- 2

initial_cluster_data <- initialize_cluster_data(x = x, 
                                                nclusters = nclusters,
                                                centers = NULL)

# initial_cluster_data


cluster_models <- estimate_normal_cluster_models(cluster_data = initial_cluster_data)

cluster_models

nclusters <- length(cluster_models)

cluster_models_parameters <- lapply(1:nclusters, function(k){
  model <- cluster_models[[k]]
  model$estimate
})

cluster_models_parameters <- do.call(what = rbind, cluster_models_parameters)

locations <- cluster_models_parameters[, "mean"]
scales <- cluster_models_parameters[, "sd"]

prior_cluster_weights <- make_weights(positives_values = rep(1, times = nclusters))

prior_cluster_weights

density_empirical <- density(x)$y

support_empirical <- density(x)$x

support <- seq(from = min(support_empirical), 
               to = max(support_empirical), 
               length.out = 1000)


density_geometric <- calculate_normal_mixture_model_pdf(x = support, 
                                                        locations = locations, 
                                                        scales = scales, 
                                                        weights = prior_cluster_weights,
                                                        kind = c("geometric", "arithmetic")[1])

density_arithmetic <- calculate_normal_mixture_model_pdf(x = support, 
                                                         locations = locations, 
                                                         scales = scales, 
                                                         weights = prior_cluster_weights,
                                                         kind = c("geometric", "arithmetic")[2])

cluster_densities <- sapply(1:length(cluster_models), function(k){
  parameters <- cluster_models_parameters[k, ]
  densities <- dnorm(x = support, 
                     mean = parameters["mean"], 
                     sd = parameters["sd"])
  densities
})

density_range <- range(c(density_empirical, 
                         density_geometric, 
                         density_arithmetic, 
                         range(cluster_densities)))


hist(x = x, 
     probability = TRUE,
     nclass = 40,
     ylim = density_range, 
     xlim = range(support))

lines(support_empirical, density_empirical, lwd = 2)

lines(support, density_geometric, col = 6, lwd = 2)
lines(support, density_arithmetic, col = 7, lwd = 2)


lapply(1:length(cluster_models), function(k){
  parameters <- cluster_models_parameters[k, ]
  densities <- dnorm(x = support, 
                     mean = parameters["mean"], 
                     sd = parameters["sd"])
  lines(x = support, y = densities, col = k + 1)
})



density_range <- range(c(density_empirical, 
                         density_geometric, 
                         density_arithmetic))


hist(x = x, 
     probability = TRUE,
     nclass = 40,
     ylim = density_range, 
     xlim = range(support))

lines(support_empirical, density_empirical, lwd = 2)

lines(support, density_geometric, col = 6, lwd = 2)
lines(support, density_arithmetic, col = 7, lwd = 2)


cluster_attractors <- calculate_normal_cluster_attractors(x = x, 
                                                          cluster_models = cluster_models, 
                                                          prior_cluster_weights = prior_cluster_weights)

names(cluster_attractors)

cluster_attractors$cluster_attractors_centers

cluster_attractors$cluster_attractors_frequencies

cluster_attractors$cluster_attractors_weights

# cluster_attractors$cluster_attractors_matrix

# cluster_attractors$cluster_data_list



# loop

# condition
current_cluster_models_parameters <- cluster_models_parameters


cluster_models <- estimate_normal_cluster_models(cluster_data = cluster_attractors$cluster_data_list)

cluster_models

cluster_attractors <- calculate_normal_cluster_attractors(x = x, 
                                                          cluster_models = cluster_models, 
                                                          prior_cluster_weights = cluster_attractors$cluster_attractors_weights)

names(cluster_attractors)

cluster_attractors$cluster_attractors_centers

cluster_attractors$cluster_attractors_frequencies

sum(cluster_attractors$cluster_attractors_frequencies)

n

cluster_attractors$cluster_attractors_weights

cluster_attractors$loglik

cluster_attractors$cluster_information_criteria

cluster_attractors$cluster_models_coefficients

cluster_attractors$cluster_models_coefficients_ci

# cluster_attractors$cluster_data_list

cluster_models_parameters <- cluster_attractors$cluster_models_coefficients

cluster_models_parameters

cluster_models_parameters_variation <- cluster_models_parameters - current_cluster_models_parameters

cluster_models_parameters_variation


locations <- cluster_models_parameters[, "mean"]
scales <- cluster_models_parameters[, "sd"]

density_empirical <- density(x)$y

support_empirical <- density(x)$x

support <- seq(from = min(support_empirical), 
               to = max(support_empirical), 
               length.out = 1000)

density_geometric <- calculate_normal_mixture_model_pdf(x = support, 
                                                        locations = locations, 
                                                        scales = scales, 
                                                        weights = prior_cluster_weights,
                                                        kind = c("geometric", "arithmetic")[1])

density_arithmetic <- calculate_normal_mixture_model_pdf(x = support, 
                                                         locations = locations, 
                                                         scales = scales, 
                                                         weights = prior_cluster_weights,
                                                         kind = c("geometric", "arithmetic")[2])


cluster_densities <- sapply(1:length(cluster_models), function(k){
  parameters <- cluster_models_parameters[k, ]
  densities <- dnorm(x = support, 
                     mean = parameters["mean"], 
                     sd = parameters["sd"])
  densities
})

density_range <- range(c(density_empirical, 
                         density_geometric, 
                         density_arithmetic, 
                         range(cluster_densities)))

hist(x = x, 
     probability = TRUE, 
     nclass = 40,
     ylim = density_range, 
     xlim = range(support))

lines(support_empirical, density_empirical, lwd = 2)

lines(support, density_geometric, col = 6, lwd = 2)
lines(support, density_arithmetic, col = 7, lwd = 2)


lapply(1:length(cluster_models), function(k){
  parameters <- cluster_models_parameters[k, ]
  densities <- dnorm(x = support, 
                     mean = parameters["mean"], 
                     sd = parameters["sd"])
  lines(x = support, y = densities, col = k + 1)
})




density_range <- range(c(density_empirical, 
                         density_geometric, 
                         density_arithmetic))


hist(x = x, 
     probability = TRUE,
     nclass = 40,
     ylim = density_range, 
     xlim = range(support))

# lines(support_empirical, density_empirical, lwd = 2)

lines(support, density_geometric, col = 6, lwd = 2)
lines(support, density_arithmetic, col = 7, lwd = 2)



