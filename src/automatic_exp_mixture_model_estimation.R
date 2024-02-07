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
source("./src/fit_stationary_normal_mixture_model.R")
source("./src/plot_fit_stationary_normal_mixture_model.R")


cluster_data_list <- lapply(1:nclusters, function(k){
  positions <- which(cluster_attractors_frequencies_table == k)
  
  cluster_data <- x[positions]
  
  cluster_data
})



# generate data from a Normal mixture model
library(mixR)
set.seed(102)
x = rmixnormal(1000, c(0.3, 0.7), c(-2, 3), c(2, 1))

mod1 = mixfit(x, ncomp = 3) 
mod1




x <- bmixture::rmixgamma(n = n, weight = c(2/4, 1/4, 1/4), alpha = c(1, 1, 1), beta = c(0.5, 1, 2))

initial_cluster_data <- initialize_cluster_data(x = x, nclusters = nclusters)


x <- rnorm(n = 1000)

Fn <- ecdf(x = x)

empirical_cdf <- Fn(x)

exponential_sample <- qexp(p = empirical_cdf, rate = 1)


hist(exponential_sample)


fit <- fitdistrplus::fitdist(data = x, distr = "exp")

fit

ed <- dexp(x = sort(x), rate = fit$estimate)


cluster_models_sizes_object <- lapply(initial_cluster_data, function(data){
  length(data)
})


cluster_models_sizes <- unlist(cluster_models_sizes_object)
cluster_models_sizes

we <- make_weights(cluster_models_sizes)



cluster_models_parameters <- lapply(initial_cluster_data, function(data){
  model <- fitdistrplus::fitdist(data = data, distr = "exp")
  model$estimate
})



extract_cluster_infos <- function(x){
  
}


vs <- unlist(cluster_models_parameters)
vs

w <- make_weights(vs)
w

de <- calculate_gamma_mixture_model_pdf(x = sort(x),
                                        scales = 1/vs,
                                        shapes = rep(x = 1, times = length(vs)),
                                        weights = w, 
                                        kind = c("geometric", "arithmetic")[1])


dee <- calculate_gamma_mixture_model_pdf(x = sort(x),
                                        scales = 1/vs,
                                        shapes = rep(x = 1, times = length(vs)),
                                        weights = we, 
                                        kind = c("geometric", "arithmetic")[1])


hist(x, probability = TRUE)

lines(sort(x), de, col = 6)

lines(sort(x), dee, col = 5)

lines(density(x))

lines(sort(x), ed, col = 3)


fit$aic

fit$bic

loglik <- sum(log(de))

logliks <- sum(log(dee))

p <- 2
q <- 1

aic <- -2*loglik + 2*(q*p + p - 1)
aic

aic <- -2*logliks + 2*(q*p + p - 1)
aic

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
                                                centers = NULL,
                                                sizes = NULL)

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

cluster_attractors$cluster_models

cluster_attractors$cluster_models_coefficients

cluster_attractors$cluster_models_coefficients_ci

cluster_attractors$selected_cluster_id


current_cluster_models_parameters <- cluster_models_parameters[cluster_attractors$selected_cluster_id, ]

cluster_models_parameters <- cluster_attractors$cluster_models_coefficients

cluster_models_parameters

cluster_models_parameters_variation <- cluster_models_parameters - current_cluster_models_parameters

cluster_models_parameters_variation


all(apply(apply(cluster_models_parameters_variation, 2, function(x) x != 0), 2, any))

all(apply(cluster_models_parameters_variation, 2, function(x) x != 0))

cluster_models_parameters_variation <- data.frame(cluster_models_parameters_variation)
cluster_models_parameters_variation <- cluster_models_parameters_variation %>% select_if(colSums(.) != 0)

is.null(ncol(dim(cluster_models_parameters_variation)))



locations <- cluster_models_parameters[1:length(cluster_attractors$selected_cluster_id), "mean"]
scales <- cluster_models_parameters[1:length(cluster_attractors$selected_cluster_id), "sd"]

density_empirical <- density(x)$y

support_empirical <- density(x)$x

support <- seq(from = min(support_empirical), 
               to = max(support_empirical), 
               length.out = 1000)

density_geometric <- calculate_normal_mixture_model_pdf(x = support, 
                                                        locations = locations, 
                                                        scales = scales, 
                                                        weights = cluster_attractors$cluster_attractors_weights,
                                                        kind = c("geometric", "arithmetic")[1])

density_arithmetic <- calculate_normal_mixture_model_pdf(x = support, 
                                                         locations = locations, 
                                                         scales = scales, 
                                                         weights = cluster_attractors$cluster_attractors_weights,
                                                         kind = c("geometric", "arithmetic")[2])


cluster_densities <- sapply(1:length(cluster_attractors$selected_cluster_id), function(k){
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


lapply(1:length(cluster_attractors$cluster_models), function(k){
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
     #nclass = 40,
     ylim = density_range, 
     xlim = range(support))

# lines(support_empirical, density_empirical, lwd = 2)

lines(support, density_geometric, col = 6, lwd = 2)
lines(support, density_arithmetic, col = 7, lwd = 2)






normal_mixture_model_object <- fit_stationary_normal_mixture_model(x = x,
                                                                   nclusters = 2,
                                                                   centers = NULL,
                                                                   sizes = NULL,
                                                                   minimum_cluster_size = 20,
                                                                   prior_cluster_weights = NULL,
                                                                   confidence_level = 0.95)

names(normal_mixture_model_object)

# [1] "x"                              "cluster_data_list"              "cluster_models"
# [4] "cluster_models_coefficients_ci" "iteration"                      "cluster_attractors_frequencies"
# [7] "cluster_attractors_weights"     "cluster_attractors_centers"     "cluster_models_coefficients"
# [10] "loglik"                         "cluster_information_criteria"

normal_mixture_model_object

plot_fit_stationary_normal_mixture_model(normal_mixture_model_object = normal_mixture_model_object,
                                         xlab = "support",
                                         ylab = "density",
                                         main = "density plot",
                                         legend_position = "topleft")

