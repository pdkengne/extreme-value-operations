# library(bmixture)
# library(factoextra)
# library(zoo)
# library(FactoMineR)


library(fitdistrplus)
library(dplyr)
library(dbscan)
library(bmixture)


source("./src/get_knn.R")
source("./src/make_weights.R")
source("./src/initialize_cluster_data.R")
source("./src/calculate_normal_cluster_attractors.R")
source("./src/calculate_normal_mixture_model_cdf.R")
source("./src/calculate_normal_mixture_model_pdf.R")
source("./src/calculate_normal_mixture_model_inverse_cdf.R")



n <- 100
x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(-10, +10), sd = c(1, 1))


nclusters <- 3

initial_cluster_data <- initialize_cluster_data(x = x, nclusters = 3)

initial_cluster_data


cluster_models <- estimate_normal_cluster_models(cluster_data = initial_cluster_data)

cluster_models

cluster_models_parameters <- do.call(what = rbind, cluster_models)

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

density_range <- range(c(density_empirical, density_geometric, density_arithmetic))


hist(x, probability = TRUE, ylim = density_range, xlim = range(support))
lines(support_empirical, density_empirical, lwd = 2)

lines(support, density_geometric, col = 6, lwd = 2)
lines(support, density_arithmetic, col = 7, lwd = 2)


lapply(1:length(cluster_models), function(k){
  parameters <- cluster_models[[k]]
  densities <- dnorm(x = support, 
                     mean = parameters["mean"], 
                     sd = parameters["sd"])
  lines(x = support, y = densities, col = k + 1)
})




cluster_attractors <- calculate_normal_cluster_attractors(x = x, 
                                                   cluster_models = cluster_models, 
                                                   prior_cluster_weights = prior_cluster_weights)

names(cluster_attractors)

cluster_attractors$cluster_attractors_centers

cluster_attractors$cluster_attractors_frequencies

cluster_attractors$cluster_attractors_weights

cluster_attractors$cluster_attractors_matrix

cluster_attractors$cluster_data_list


# loop

cluster_models <- estimate_normal_cluster_models(cluster_data = cluster_attractors$cluster_data_list)

cluster_models

support <- seq(from = min(x), to = max(x), by = 1)
hist(x, probability = TRUE)
lines(density(x))

lapply(1:length(cluster_models), function(k){
  parameters <- cluster_models[[k]]
  densities <- dnorm(x = support, 
                     mean = parameters["mean"], 
                     sd = parameters["sd"])
  lines(x = support, y = densities, col = k + 1)
})


cluster_attractors <- calculate_normal_cluster_attractors(x = x, 
                                                   cluster_models = cluster_models, 
                                                   prior_cluster_weights = cluster_attractors$cluster_attractors_weights)

names(cluster_attractors)

cluster_attractors$cluster_attractors_centers

cluster_attractors$cluster_attractors_frequencies

cluster_attractors$cluster_attractors_weights

cluster_attractors$cluster_attractors_matrix

cluster_attractors$cluster_data_list





















