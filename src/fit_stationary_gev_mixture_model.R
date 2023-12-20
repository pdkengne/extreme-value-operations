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
  else{
    candidate_block_sizes <- block_sizes
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
  
  # extract the unselected block sizes
  unselected_block_sizes <- automatic_weights_object$unselected_block_sizes
  
  # extract the labels of selected models
  selected_model_labels <- automatic_weights_object$selected_model_labels
  
  # extract the vector of selected models per observation
  selected_model_per_obs <- automatic_weights_object$selected_model_per_obs
  
  # extract the vector of frequencies
  frequencies <- as.numeric(table(selected_model_per_obs))
  names(frequencies) <- selected_block_sizes
  
  # extract the vector of weights
  weights <- automatic_weights_object$weights
  names(weights) <- selected_block_sizes
  
  # extract all estimated gev model objects
  gev_models_objects <- gev_models$gev_models_object
    
  # extract the selected gev models
  selected_gev_models <- lapply(selected_model_labels, function(k){
    model <- gev_models_objects[[k]]
    model
  })
  
  names(selected_gev_models) <- selected_block_sizes
  
  # extract the extremal indexes associated with the selected gev models
  extremal_indexes <- gev_models$extremal_indexes[selected_model_labels]
  
  # extract the normalized gev parameters associated with the selected gev models
  normalized_gev_parameters_object <- gev_models$normalized_gev_parameters_object[selected_model_labels, ]
  
  # extract the full normalized gev parameters associated with the selected gev models
  full_normalized_gev_parameters_object <- gev_models$full_normalized_gev_parameters_object[selected_model_labels, ]
  
  # extract the negative log-likelihood associated with the selected gev models
  nllh <- sapply(selected_gev_models, function(model){
    res <- summary(model, silent = TRUE)
    res$nllh
  })
  
  names(nllh) <- selected_block_sizes
  
  # calculate the information criteria, namely aic and bic
  p <- nrow(normalized_gev_parameters_object)
  q <- ncol(normalized_gev_parameters_object)
  
  aic <- 2*sum(nllh) + 2*(q*p + p - 1)
  bic <- 2*sum(nllh) + log(n)*(q*p + p - 1)
  
  information_criteria <- c(aic, bic)
  names(information_criteria) <- c("AIC", "BIC")
  
  # update the output object
  output[["equivalent_block_sizes"]] <- equivalent_block_sizes
  output[["unequivalent_block_sizes"]] <- unequivalent_block_sizes
  output[["selected_block_sizes"]] <- selected_block_sizes
  output[["unselected_block_sizes"]] <- unselected_block_sizes
  output[["weights"]] <- weights
  output[["frequencies"]] <- frequencies
  output[["use_extremal_index"]] <- use_extremal_index
  output[["extremal_indexes"]] <- extremal_indexes
  output[["negative_log_likelihoods"]] <- nllh
  output[["information_criteria"]] <- information_criteria
  output[["normalized_gev_parameters_object"]] <- normalized_gev_parameters_object
  output[["full_normalized_gev_parameters_object"]] <- full_normalized_gev_parameters_object
  output[["selected_model_per_obs"]] <- selected_model_per_obs
  output[["data"]] <- x
  output[["selected_gev_models"]] <- selected_gev_models
  
  output
}


# example 1

source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_stationary_gev_mixture_model.R")
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

results <- fit_stationary_gev_mixture_model(x = x, 
                                            block_sizes = 50,
                                            minimum_nblocks = 50,
                                            threshold = NULL,
                                            confidence_level = 0.95,
                                            use_extremal_index = TRUE,
                                            use_uniform_prior = TRUE,
                                            method = c("MLE", "GMLE", "Lmoments")[1])

names(results)

# [1] "equivalent_block_sizes"                "unequivalent_block_sizes"              "selected_block_sizes"                 
# [4] "unselected_block_sizes"                "weights"                               "frequencies"                          
# [7] "use_extremal_index"                    "extremal_indexes"                      "negative_log_likelihoods"             
# [10] "information_criteria"                  "normalized_gev_parameters_object"      "full_normalized_gev_parameters_object"
# [13] "selected_model_per_obs"                "data"                                  "selected_gev_models"


results$equivalent_block_sizes

results$unequivalent_block_sizes

results$selected_block_sizes

results$unselected_block_sizes

results$weights

results$frequencies

results$use_extremal_index

results$extremal_indexes

results$negative_log_likelihoods

results$information_criteria

results$normalized_gev_parameters_object

results$full_normalized_gev_parameters_object

results$selected_model_per_obs

results$data

results$selected_gev_models



plot_fit_stationary_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")




# example 2

source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_stationary_gev_mixture_model.R")
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


plot_fit_stationary_gev_mixture_model(gev_mixture_model_object = results,
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


plot_fit_stationary_gev_mixture_model(gev_mixture_model_object = final_results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")


lines(sort(x), dens, col = 4, lwd = 2)



# example 3


source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_stationary_gev_mixture_model.R")

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


plot_fit_stationary_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")

abline(h = 0, lty = "dotted")


# example 4

source("./src/calculate_modes.R")
source("./src/plot_modes.R")
source("./src/plot_fit_stationary_gev_mixture_model.R")

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


plot_fit_stationary_gev_mixture_model(gev_mixture_model_object = results,
                           xlab = "support",
                           ylab = "density",
                           main = "density plot",
                           legend_position = "topright")



