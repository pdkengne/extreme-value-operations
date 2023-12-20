# library(extRemes) 
# library(fitdistrplus)

source("./src/get_candidate_block_sizes.R")
source("./src/estimate_several_gev_models.R")
source("./src/estimate_several_standardized_block_maxima_mean.R")
source("./src/estimate_gev_mixture_model_automatic_weights.R")


fit_stationary_gev_mixture_model <- function(x, 
                                             block_sizes = NULL,
                                             minimum_nblocks = 50,
                                             threshold = NULL,
                                             confidence_level = 0.95,
                                             use_extremal_index = TRUE,
                                             use_uniform_prior = TRUE,
                                             method = c("MLE", "GMLE", "Lmoments")[1]){
  # x: vector of observations
  # block_sizes: vector containing the sizes of blocks to consider
  # threshold: lower bound of block maxima
  # use_extremal_index: a boolean which indicates whether to use the estimates extremal indexes or not
  # use_uniform_prior: a boolean which indicates whether to use a uniform prior (TRUE) or a pessimistic prior (FALSE)
  # confidence_level: desired confidence level when extraction equivalent block sizes. 
  #                   Note that this value is ignored if block_sizes != NULL.
  # minimum_nblocks: desired minimum number of blocks. Note that this number is used to infer the largest block size.
  #                  Moreover, this number is ignored if block_sizes != NULL.
  # method: estimation method to use
  
  # create an empty output object
  output <- list()
  
  # get candidate block sizes
  if (is.null(block_sizes)){
    candidate_block_sizes <- get_candidate_block_sizes(x = x, 
                                                       threshold = threshold, 
                                                       m = minimum_nblocks)
  }
  
  # get equivalent block sizes
  equivalent_block_sizes_object <- estimate_several_standardized_block_maxima_mean(x = x, 
                                                                                  block_sizes = candidate_block_sizes, 
                                                                                  confidence_level = confidence_level,
                                                                                  method = method)
  
  equivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$selected))
  
  # get eventual rejected block sizes from the equivalent ones
  unequivalent_block_sizes <- as.numeric(rownames(equivalent_block_sizes_object$rejected))
  
  # estimate several gev models associated with the equivalent block sizes
  gev_models <- estimate_several_gev_models(x = x, 
                                            block_sizes = equivalent_block_sizes,
                                            method = method)
  
  # estimate the gev model weights
  automatic_weights_object <- estimate_gev_mixture_model_automatic_weights(gev_models = gev_models,
                                                                           use_uniform_prior = use_uniform_prior,
                                                                           use_extremal_index = use_extremal_index)
  # extract the selected block sizes
  selected_block_sizes <- automatic_weights_object$selected_block_sizes
  
  # extract the unweighted block sizes
  unweighted_block_sizes <- automatic_weights_object$unweighted_block_sizes
  
  
  
  #--------------------
  z <- apply(posterior, 2, which.max)
  clusters_freq <- table(z)
  clusters_labels <- as.numeric(names(clusters_freq))
  omega <- as.numeric(prop.table(clusters_freq))
  #--------------------
  
  
  
  final_models <- lapply(clusters_labels, function(k){
    y <-  extract_block_maxima(x, block_size = block_sizes[k])
    model <- extRemes::fevd(x = y, type = "GEV")
    model
  })
  
  parameters <- sapply(final_models, function(model){
    model$results$par
  })
  
  parameters_star <- sapply(1:length(clusters_labels), function(k){
    estimates <- parameters[, k]
    estimates_star <- calculate_power_gev_parameters(loc = estimates["location"],
                                                     scale = estimates["scale"],
                                                     shape = estimates["shape"],
                                                     exponent = 1/selected_block_sizes[k])
    estimates_star
  })
  
  nllh <- sapply(final_models, function(model){
    res <- summary(model, silent = TRUE)
    res$nllh
  })
  
  names(nllh) <- clusters_labels
  
  p <- length(clusters_labels)
  q <- nrow(theta)
  
  aic <- 2*sum(nllh) + 2*(q*p + p)
  bic <- 2*sum(nllh) + log(n)*(q*p + p)
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
  output[["cluster_gev_model_parameters_star"]] <- parameters_star
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

#x <- rnorm(n = 3000)

#x <- rexp(n = 3000)

n <- 3000

loc <- 0
scale <- 1
shape <- 0.01

x <- extRemes::revd(n = n, loc = loc, scale = scale, shape = shape)

dens <- extRemes::devd(x = sort(x), loc = loc, scale = scale, shape = shape)

modes_object <- calculate_modes(x = x)

plot_modes(modes_object)

results <- fit_stationary_gev_mixture_model(x = x, block_sizes = c(10:50))

names(results)

# [1] "nclusters"                         "cluster_sizes"                     "selected_block_sizes"              "cluster_weights"                   "cluster_negative_loglikelihoods"  
# [6] "information_criterions"            "cluster_gev_model_parameters"      "cluster_gev_model_parameters_star" "clusters"                          "data"                             
# [11] "cluster_models"


results$nclusters

results$cluster_sizes

results$selected_block_sizes

results$information_criterions

results$cluster_gev_model_parameters

results$cluster_gev_model_parameters_star

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
source("./src/plot_several_standardized_block_maxima_mean.R")


x <- bmixture::rmixnorm(n = 3000, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))

modes_object <- calculate_modes(x = x)

plot_modes(modes_object)


results <- fit_stationary_gev_mixture_model(x = x, block_sizes = c(3:50))

names(results)

results$nclusters

results$cluster_sizes

results$selected_block_sizes

results$information_criterions

results$cluster_gev_model_parameters

results$cluster_gev_model_parameters_star

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


final_results <- fit_stationary_gev_mixture_model(x = x, block_sizes = blocks)

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

results <- fit_stationary_gev_mixture_model(x = z, block_sizes = c(1:5))

names(results)

results$nclusters

results$cluster_sizes

results$cluster_weights

results$information_criterions

results$cluster_gev_model_parameters

results$cluster_gev_model_parameters_star

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

results <- fit_stationary_gev_mixture_model(x = x,
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



