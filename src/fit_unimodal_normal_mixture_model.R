# library(fitdistrplus)

fit_unimodal_normal_mixture_model <- function(x, 
                                       minimum_size = 50,
                                       method = c("mle", "mme", "qme", "mge", "mse")[1]){
  # x: vector of observations
  # minimum_size: indicates the minimum sample size to consider
  # method: estimation method to use 
  
  # create an empty output object
  output <- list()
  
  n <- length(x)
  
  p <- floor(n/minimum_size)
  
  omega <- rep(x = 1, times = p)/p
  
  data_index <- 1:n
  sample_index <- 1:p 
  
  theta <- sapply(sample_index, function(k){
    y <-  x[which(data_index %% k == 0)]
    model <- fitdistrplus::fitdist(data = y, distr = "norm", method = method)
    model$estimate
  })
  
  posterior <- sapply(data_index, function(i){
    obs <- x[i]
    likelihood <- sapply(sample_index, function(k){
      dens <- dnorm(x = obs, 
                    mean = theta["mean", k], 
                    sd = theta["sd", k])
      dens*omega[k]
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
  
  selected_sample_index <- sample_index[clusters_labels]
  
  final_models <- lapply(clusters_labels, function(k){
    y <-  x[which(data_index %% k == 0)]
    model <- fitdistrplus::fitdist(data = y, distr = "norm", method = method)
    model
  })
  
  parameters <- sapply(final_models, function(model){
    model$estimate
  })
  
  nllh <- sapply(final_models, function(model){
    - model$loglik
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
  output[["selected_sample_index"]] <- selected_sample_index
  output[["cluster_weights"]] <- omega
  output[["cluster_negative_loglikelihoods"]] <- nllh
  output[["information_criterions"]] <- information_criterions
  output[["cluster_model_parameters"]] <- parameters
  output[["clusters"]] <- z
  output[["data"]] <- x
  output[["cluster_models"]] <- final_models
  
  output
}



# # example 1
# 
# source("./src/calculate_normal_mixture_model_pdf.R")
# source("./src/generate_normal_mixture_model_sample.R")
# 
# x <- rnorm(n = 10000, mean = 0, sd = 1)
# 
# # x <- generate_normal_mixture_model_sample(n = 2000,
# #                                           locations = c(-4, +4, +12),
# #                                           scales = c(1, 1, 1),
# #                                           weights = c(1/3, 1/3, 1/3),
# #                                           kind = c("geometric", "arithmetic")[2])
# 
# results <- fit_unimodal_normal_mixture_model(x = x, 
#                                              minimum_size = 50,
#                                              method = c("mle", "mme", "qme", "mge", "mse")[1])
# 
# #names(results)
# 
# # [1] "nclusters"                       "cluster_sizes"                   "selected_sample_index"           "cluster_weights"                 "cluster_negative_loglikelihoods"
# # [6] "information_criterions"          "cluster_model_parameters"        "clusters"                        "data"                            "cluster_models" 
# 
# results$nclusters
# 
# results$cluster_sizes
# 
# results$selected_sample_index
# 
# results$information_criterions
# 
# results$cluster_model_parameters
# 
# results$cluster_weights
# 
# #results$cluster_models
# 
# 
# parameters <- results$cluster_model_parameters
# 
# locations <- parameters["mean", ]
# scales <- parameters["sd", ]
# weights <- results$cluster_weights
# 
# dens_empirical <- density(x)$y
# 
# support <- density(x)$x
# 
# dens_arithmetic <- calculate_normal_mixture_model_pdf(x = support,
#                                                       locations = locations,
#                                                       scales = scales,
#                                                       weights =  weights,
#                                                       kind = c("geometric", "arithmetic")[2])
# 
# dens_geometric <- calculate_normal_mixture_model_pdf(x = support,
#                                                      locations = locations,
#                                                      scales = scales,
#                                                      weights =  weights,
#                                                      kind = c("geometric", "arithmetic")[1])
# 
# densities <- c(dens_empirical, dens_arithmetic, dens_geometric)
# 
# plot(density(x), 
#      ylim = range(densities),
#      lwd = 2)
# 
# lines(x = support, 
#       y = dens_arithmetic,
#       lwd = 2,
#       col = 7)
# 
# lines(x = support,
#       y = dens_geometric,
#       lwd = 2,
#       col = 6)
# 
# 
# fit <- fitdistrplus::fitdist(data = x, distr = "norm", method = "mle")
# 
# dens_single <- dnorm(x = support, mean = fit$estimate["mean"], sd = fit$estimate["sd"])
# 
# lines(x = support,
#       y = dens_single,
#       lwd = 2,
#       col = 4)
# 
# dens_true <- dnorm(x = support, mean = 0, sd = 1)
# 
# lines(x = support,
#      y = dens_true,
#      lwd = 2,
#      col = 3)
# 
# 
# fit$estimate
# parameters
# 
# summary(fit)
# results$information_criterions

