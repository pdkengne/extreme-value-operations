source("./src/estimate_ns_gev_parameters.R")


estimate_ns_evd_cluster_models <- function(input_cluster_data,
                                           location.fun = ~1,
                                           scale.fun = ~ 1, 
                                           shape.fun = ~1, 
                                           use.phi = TRUE,
                                           type = c("GEV", "Gumbel")[1],
                                           method = c("MLE", "GMLE")[1]){
  # input_cluster_data:
  # location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                     must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter in numerical optimization
  # type: type of model to use
  # method: estimation method to use:
  
  cluster_data <- input_cluster_data$cluster_data
  cluster_covariates <- input_cluster_data$cluster_covariates
  
  nclusters <- length(cluster_data)
  
  cluster_models <- lapply(1:nclusters, function(k){
    x <- cluster_data[[k]]
    data <- cluster_covariates[[k]]
    
    model <- estimate_ns_gev_parameters(x = x,
                                        data = data, 
                                        location.fun = location.fun,
                                        scale.fun = scale.fun, 
                                        shape.fun = shape.fun, 
                                        use.phi = use.phi,
                                        type = type,
                                        method = method)
    model
  })
  
  cluster_models
}


# # example 1
# 
# source("./src/generate_gev_sample.R")
# source("./src/initialize_ns_cluster_data.R")
# 
# n <- 1000
# x <- bmixture::rmixnorm(n = n, weight = c(1/2, 1/2), mean = c(0.6, 1.3), sd = c(0.1, 0.1))
# 
# #x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = -0.2)
# 
# trend <- 1:n/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# covariates <- data.frame(trend = trend, random = rnd)
# 
# hist(x, nclass = NULL)
# 
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_ns_cluster_data(x = x,
#                                                    covariates = covariates,
#                                                    nclusters = nclusters)
# 
# names(initial_cluster_data)
# 
# initial_cluster_data$cluster_data
# 
# initial_cluster_data$cluster_covariates
# 
# 
# cluster_models <- estimate_ns_evd_cluster_models(input_cluster_data = initial_cluster_data,
#                                                  location.fun = ~ random,
#                                                  scale.fun = ~ 1,
#                                                  shape.fun = ~1)
# 
# cluster_models
# 
# 
# cluster_models <- estimate_ns_evd_cluster_models(input_cluster_data = initial_cluster_data,
#                                                  location.fun = ~ 1,
#                                                  scale.fun = ~ 1,
#                                                  shape.fun = ~1)
# 
# cluster_models
# 
# 
# # example 2
# 
# source("./src/initialize_ns_cluster_data.R")
# source("./src/generate_gev_mixture_model_sample.R")
# 
# n <- 1000
# x <- generate_gev_mixture_model_sample(n = n,
#                                        weights = c(1/2, 1/2),
#                                        locations = c(-3, +3),
#                                        scales = c(1, 1),
#                                        shapes = c(-0.01, +0.01),
#                                        kind = c("geometric", "arithmetic", "harmonic")[2])
# 
# hist(x, nclass = 30)
# 
# trend <- 1:n/n
# rnd <- runif(n = n, min = -0.5, max = 0.5)
# covariates <- data.frame(trend = trend, random = rnd)
# 
# 
# nclusters <- 2
# 
# initial_cluster_data <- initialize_ns_cluster_data(x = x,
#                                                    covariates = covariates,
#                                                    nclusters = nclusters)
# 
# names(initial_cluster_data)
# 
# initial_cluster_data$cluster_data
# 
# initial_cluster_data$cluster_covariates
# 
# 
# cluster_models <- estimate_ns_evd_cluster_models(input_cluster_data = initial_cluster_data,
#                                                  location.fun = ~ random,
#                                                  scale.fun = ~ 1,
#                                                  shape.fun = ~1)
# 
# cluster_models
