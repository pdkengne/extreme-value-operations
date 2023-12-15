# library(fitdistrplus)

library(extRemes)

fit_unimodal_evd_mixture_model <- function(x, 
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
    model <- fitdistrplus::fitdist(data = y, 
                                   distr = "evd", 
                                   method = method,
                                   start = list("loc" = mean(x),
                                                "scale" = sd(x),
                                                "shape" = 0.001))
    model$estimate
  })
  
  # posterior <- sapply(data_index, function(i){
  #   obs <- x[i]
  #   likelihood <- sapply(sample_index, function(k){
  #     dens <- devd(x = obs, 
  #                  loc = theta["loc", k], 
  #                  scale = theta["scale", k],
  #                  shape = theta["shape", k],)
  #     dens*omega[k]
  #   })
  #   likelihood/sum(likelihood)
  # })
  
  s <- floor(log(n)/log(2))
  m <- 2^s
  empirical_density_object <- density(x = x, n = m)
  support <- empirical_density_object$x
  empirical_density <- empirical_density_object$y
  
  density_index <- 1:length(support)
  
  posterior <- sapply(density_index, function(i){
    obs <- support[i]
    likelihood <- sapply(sample_index, function(k){
      dens <- devd(x = obs, 
                   loc = theta["loc", k], 
                   scale = theta["scale", k],
                   shape = theta["shape", k],)
      dens
    })
    abs(likelihood - empirical_density[i])
  })
  
  posterior_nb_na <- sum(is.na(posterior))
  
  if (posterior_nb_na != 0){
    stop("Sorry, algorithm does not converge with the current inputs !")
  }
  
  
  #--------------------
  z <- apply(posterior, 2, which.min)
  clusters_freq <- table(z)
  clusters_labels <- as.numeric(names(clusters_freq))
  omega <- as.numeric(prop.table(clusters_freq))
  #--------------------
  
  selected_sample_index <- sample_index[clusters_labels]
  
  final_models <- lapply(clusters_labels, function(k){
    y <-  x[which(data_index %% k == 0)]
    model <- fitdistrplus::fitdist(data = y, 
                                   distr = "evd", 
                                   method = method,
                                   start = list("loc" = mean(x),
                                                "scale" = sd(x),
                                                "shape" = 0.001))
    model
  })
  
  parameters <- sapply(final_models, function(model){
    model$estimate
  })
  
  nllh <- sapply(final_models, function(model){
    -1*model$loglik
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



# example 1

source("./src/calculate_gev_mixture_model_pdf.R")

x <- rnorm(n = 3000, mean = 0, sd = 1)

x <- revd(n = 3000, loc= 0, scale = 1, shape = 0.001)

x <- rexp(n = 3000)

results <- fit_unimodal_evd_mixture_model(x = x, 
                                          minimum_size = 50,
                                          method = c("mle", "mme", "qme", "mge", "mse")[1])

#names(results)

# [1] "nclusters"                       "cluster_sizes"                   "selected_sample_index"           "cluster_weights"                 "cluster_negative_loglikelihoods"
# [6] "information_criterions"          "cluster_model_parameters"        "clusters"                        "data"                            "cluster_models" 

results$nclusters

results$cluster_sizes

results$selected_sample_index

results$information_criterions

results$cluster_model_parameters

results$cluster_weights

#results$cluster_models


parameters <- results$cluster_model_parameters

locations <- parameters["loc", ]
scales <- parameters["scale", ]
shapes <- parameters["shape", ]
weights <- results$cluster_weights


n <- length(x)
s <- floor(log(n)/log(2))
m <- 2^s

density_object <-  density(x, n = m)

dens_empirical <- density_object$y

support <- density_object$x

dens_arithmetic <- calculate_gev_mixture_model_pdf(x = support,
                                                   locations = locations,
                                                   scales = scales,
                                                   shapes = shapes,
                                                   weights =  weights,
                                                   kind = c("geometric", "arithmetic")[2])

dens_geometric <- calculate_gev_mixture_model_pdf(x = support,
                                                  locations = locations,
                                                  scales = scales,
                                                  shapes = shapes,
                                                  weights =  weights,
                                                  kind = c("geometric", "arithmetic")[1])

densities <- c(dens_empirical, dens_arithmetic, dens_geometric)

plot(x = support,
     y = dens_empirical, 
     ylim = range(densities),
     type = "l",
     lwd = 2)

lines(x = support, 
      y = dens_arithmetic,
      lwd = 2,
      col = 7)

lines(x = support,
      y = dens_geometric,
      lwd = 2,
      col = 6)


fit <- fitdistrplus::fitdist(data = x, 
                             distr = "evd", 
                             method = "mle",
                             start = list("loc" = mean(x),
                                          "scale" = sd(x),
                                          "shape" = 0.001))


fit <- fitdistrplus::fitdist(data = x, 
                             distr = "norm", 
                             method = "mle")


fit <- fitdistrplus::fitdist(data = x, 
                             distr = "exp", 
                             method = "mle")

dens_single <- devd(x = support, 
                    loc = fit$estimate["loc"], 
                    scale = fit$estimate["scale"],
                    shape = fit$estimate["shape"])


dens_single <- dnorm(x = support, 
                    mean = fit$estimate["mean"], 
                    sd = fit$estimate["sd"])


dens_single <- dexp(x = support, 
                     rate = fit$estimate["rate"])

lines(x = support,
      y = dens_single,
      lwd = 2,
      col = 4)


dens_true <- devd(x = support, 
                  loc= 0, 
                  scale = 1, 
                  shape = 0.001)


dens_true <- dnorm(x = support, 
                   mean= 0, 
                   sd = 1)

dens_true <- dexp(x = support, 
                   rate= 1)


lines(x = support,
      y = dens_true,
      lwd = 2,
      col = 3)


#fit$estimate
#parameters

summary(fit)
results$information_criterions

range(shapes)

which.max(shapes)
