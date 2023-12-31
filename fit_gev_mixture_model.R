#-------------------------------------------------------------------------------
setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")
#-------------------------------------------------------------------------------

library(tidyverse)

library(DescTools)

library(maxLik)

library(extRemes)

library(bmixture)

library(ggplot2)

library(Hmisc)

library(extraDistr)



#-------------------------------------------------------------------------------

source("./src/calculate_modes.R")

fit_gev_mixture_model <- function(x, 
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
  
  if (p > 1){
    modes_object <- calculate_modes(x = x)
    density_minima <- modes_object$density_minima 
    density_minima_selected <- tail(sort(density_minima), n = p - 1)
    breaks = modes_object$density_minima_argument
    breaks <- c(min(x), breaks[density_minima %in% density_minima_selected], max(x))
    z <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
  } else{
    z <- rep(x = 1, times = n)
  }
  
  clusters_freq <- table(z)
  clusters_labels <- as.numeric(names(clusters_freq))
  omega <- as.numeric(prop.table(clusters_freq))
  
  while (min(as.numeric(clusters_freq)) < min_cluster_size & p > 1){
    p <- p - 1
    
    modes_object <- calculate_modes(x = x)
    density_minima <- modes_object$density_minima 
    density_minima_selected <- tail(sort(density_minima), n = p - 1)
    breaks = modes_object$density_minima_argument
    breaks <- c(min(x), breaks[density_minima %in% density_minima_selected], max(x))
    z <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
    
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
      
      modes_object <- calculate_modes(x = x)
      density_minima <- modes_object$density_minima 
      density_minima_selected <- tail(sort(density_minima), n = p - 1)
      breaks = modes_object$density_minima_argument
      breaks <- c(min(x), breaks[density_minima %in% density_minima_selected], max(x))
      z <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
      
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
  output[["cluster_models"]] <- final_models
  
  output
}




#-------------------------------------------------------------------------------
source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/generate_gev_mixture_model_sample.R")

weights <- c(0.3, 0.8)

shapes <- c(0.1, 0.1)
scales <- c(1, 1)
locations <- c(-2, +2)


weights <- c(1/3, 1/3, 1/3)

shapes <- c(0.1, 0.1, 0.1)
scales <- c(1, 1, 1)
locations <- c(-2, +2, +6)

n <- 5000

x <- generate_gev_mixture_model_sample(n = n,
                                       locations,
                                       scales,
                                       shapes,
                                       weights,
                                       kind = c("geometric", "arithmetic")[2])


# x <- extRemes::revd(n = n, loc = 1, scale = 1, shape = 0.2, type = "GEV")

# x <- bmixture::rmixnorm(n = n, weight = c(0.5, 0.5), mean = c(-5, 0), sd = c(1, 1))

p <- 3

results <- fit_gev_mixture_model(x, 
                                 nb_gev_models = p, 
                                 min_cluster_size = 20, 
                                 max_iteration = 40,
                                 left_cluster_extension_size = 10,
                                 right_cluster_extension_size = 40,
                                 tolerance = 10^(-3))

results

#-------------------------------------------------------------------------------

# plot 1 component

support <- sort(x)

model_1 <- results$cluster_models[[1]]
par_model_1 <- model_1$results$par
dens_1 <- extRemes::devd(x = support, loc = par_model_1[1], scale = par_model_1[2], shape = par_model_1[3])

parameters <- rbind(par_model_1)
weights <- results$cluster_weights

densities <- c(dens_1)

hist(x, 
     freq = FALSE, 
     ylim = range(densities),
     main = "Density plots")
lines(support, dens_1, col = 3, lwd = 2)

lines(density(x), col = 1, lwd = 2)

legend("topright", 
       legend = c("empirical", "gev_1"),
       lty = c(1, 1), col = c(1, 3))


#-------------------------------------------------------------------------------

# plot 2 components

support <- sort(x)

model_1 <- results$cluster_models[[1]]
par_model_1 <- model_1$results$par
dens_1 <- extRemes::devd(x = support, loc = par_model_1[1], scale = par_model_1[2], shape = par_model_1[3])



model_2 <- results$cluster_models[[2]]
par_model_2 <- model_2$results$par
dens_2 <- extRemes::devd(x = support, loc = par_model_2[1], scale = par_model_2[2], shape = par_model_2[3])


parameters <- rbind(par_model_1, par_model_2)
weights <- results$cluster_weights

pdf_1 <- calculate_gev_mixture_model_pdf(x = support,
                                       locations = parameters[, 1],
                                       scales =  parameters[, 2],
                                       shapes = parameters[, 3],
                                       weights = weights,
                                       kind = c("geometric", "arithmetic")[1])


pdf_2 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations = parameters[, 1],
                                         scales =  parameters[, 2],
                                         shapes = parameters[, 3],
                                         weights = weights,
                                         kind = c("geometric", "arithmetic")[2])


densities <- c(dens_1, dens_2, pdf_1, pdf_2)

hist(x, 
     freq = FALSE, 
     ylim = range(densities),
     main = "Density plots")
lines(support, dens_1, col = 3, lwd = 2, lty = "dotted")
lines(support, dens_2, col = 4, lwd = 2, lty = "dotted")

lines(density(x), col = 1, lwd = 2)

lines(support, pdf_1, col = 6, lwd = 2)
lines(support, pdf_2, col = 7, lwd = 2)

legend("topright", 
       legend = c("empirical", "gev_1", "gev_2", "mix_gev_sum", "mix_gev_product"),
       lty = c(1, 2, 2, 1, 1), col = c(1, 3, 4, 7, 6))

#-------------------------------------------------------------------------------

# plot 3 components


support <- sort(x)

model_1 <- results$cluster_models[[1]]
par_model_1 <- model_1$results$par
dens_1 <- extRemes::devd(x = support, loc = par_model_1[1], scale = par_model_1[2], shape = par_model_1[3])



model_2 <- results$cluster_models[[2]]
par_model_2 <- model_2$results$par
dens_2 <- extRemes::devd(x = support, loc = par_model_2[1], scale = par_model_2[2], shape = par_model_2[3])


model_3 <- results$cluster_models[[3]]
par_model_3 <- model_3$results$par
dens_3 <- extRemes::devd(x = support, loc = par_model_3[1], scale = par_model_3[2], shape = par_model_3[3])


parameters <- rbind(par_model_1, par_model_2, par_model_3)
weights <- results$cluster_weights

pdf_1 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations = parameters[, 1],
                                         scales =  parameters[, 2],
                                         shapes = parameters[, 3],
                                         weights = weights,
                                         kind = c("geometric", "arithmetic")[1])


pdf_2 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations = parameters[, 1],
                                         scales =  parameters[, 2],
                                         shapes = parameters[, 3],
                                         weights = weights,
                                         kind = c("geometric", "arithmetic")[2])


densities <- c(dens_1, dens_2, dens_3, pdf_1, pdf_2)

hist(x, 
     freq = FALSE, 
     ylim = range(densities),
     main = "Density plots")
lines(support, dens_1, col = 3, lwd = 2, lty = "dotted")
lines(support, dens_2, col = 4, lwd = 2, lty = "dotted")
lines(support, dens_3, col = 5, lwd = 2, lty = "dotted")

lines(density(x), col = 1, lwd = 2)

lines(support, pdf_1, col = 6, lwd = 2)
lines(support, pdf_2, col = 7, lwd = 2)

legend("topright", 
       legend = c("empirical", "gev_1", "gev_2", "gev_3", "mix_gev_sum", "mix_gev_product"),
       lty = c(1, 2, 2, 2, 1, 1), col = c(1, 3, 4, 5, 7, 6))




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------





n <- 100

x <- extRemes::revd(n = n, loc = 1, scale = 1, shape = 0.2, type = "GEV")

p <- 4

min_cluster_size <- 10

max_iteration <- 1500

tolerance <- 10^(-3)


# make n groups with (approximately) equal numbers of observations
z <- as.numeric(ggplot2::cut_number(x = x, n = p))

table(z)

# generate random weights
omega <- bmixture::rdirichlet(n = 1, alpha = rep(x = 1, times = p))[1, ]

omega

model_initial <- extRemes::fevd(x = x, type = "GEV")

res <- summary(model_initial)

names(res)

res$AIC
res$BIC

parameters <- model_initial$results$par

parameters

theta <- sapply(1:p, function(k){
  y <- x[which(z == k)]

  model <- extRemes::fevd(x = y, type = "GEV")
  
  model$results$par
})

aic_vector <- c(aic_vector, sum(res_aic))
bic_vector <- c(res_bic, sum(res_bic))



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


posterior

z <- apply(posterior, 2, which.max)

clusters_freq <- table(z)

clusters_labels <- as.numeric(names(clusters_freq))

p <- length(clusters_labels)

if (p > 1){
  if (min(as.numeric(clusters_freq)) < min_cluster_size){
    p <- p - 1
    z <- as.numeric(ggplot2::cut_number(x = x, n = p))
    clusters_freq <- table(z)
    clusters_labels <- as.numeric(names(clusters_freq))
    omega <- bmixture::rdirichlet(n = 1, alpha = rep(x = 1, times = p))[1, ]
  } else{
    omega <- as.numeric(prop.table(clusters_freq))
  }
} else{
  z <- rep(x = 1, times = n)
  clusters_freq <- table(z)
  clusters_labels <- as.numeric(names(clusters_freq))
  omega <- bmixture::rdirichlet(n = 1, alpha = rep(x = 1, times = p))[1, ]
}


theta <- sapply(clusters_labels, function(k){
  y <- x[which(z == k)]
  
  model <- extRemes::fevd(x = y, type = "GEV")
  
  model$results$par
})

theta

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


posterior







x <- revd(n = 1000, loc = 1, scale = 1, shape = 0.2, type = "GEV")

m <- maxLik(logLik = loglik, 
            start = c(mu = mean(x), phi = log(sd(x)), gamma = -0.001), 
            method = c("NR", "BFGS")[1],
            x = x)

summary(m)


#-------------------------------------------------------------------------------

loglik_em <- function(theta, x, weights){
  mu <- theta[1]
  phi <- theta[2]
  gamma <- theta[3]
  
  sigma <- exp(phi)
  
  terms <- extRemes::devd(x = x, loc = mu, scale = sigma, shape = gamma, log = TRUE, type = "GEV")
  
  sum(weights*terms)
}


n <- 1000

x <- revd(n = n, loc = 1, scale = 1, shape = 0.2, type = "GEV")

x <- bmixture::rmixnorm(n = n, weight = weight, mean = mean, sd = sd)

p <- 3

# omega <- rep(x = 1/p, times = p)

# locations <- rep(x = mean(x), times = p)

# log_scales <- rep(x = log(sd(x)), times = p)

# shapes <- rep(x = -0.001, times = p)

omega <- bmixture::rdirichlet(n = 1, alpha = c(1, 1, 1))

locations <- c(1, 5, 10)

log_scales <- c(0.1, 0.2, 0.3)

shapes <- c(0.1, 0.2, 0.3)



posterior <- sapply(1:n, function(i){
  obs <- x[i]
  
  likelihood <- sapply(1:p, function(k){
    mu <- locations[k]
    sigma <- exp(log_scales[k])
    gamma <- shapes[k]
    dens <- extRemes::devd(x = obs, 
                           loc = mu, 
                           scale = sigma, 
                           shape = gamma, 
                           log = FALSE, 
                           type = "GEV")
    
    prior <- omega[k]
    
    prior*dens
  })
  
  likelihood/sum(likelihood)
})


#posterior

omega <- apply(posterior, 1, mean)

omega

theta <- sapply(1:p, function(k){
  weights <- posterior[k, ]
  
  model <- maxLik(logLik = loglik_em, 
                    start = c(mu = sum(weights*x), phi = log(sd(weights*x)), gamma = -0.001), 
                    method = c("NR", "BFGS")[1],
                    x = x,
                    weights = weights)
  
  coefficients(model)
})

theta

locations <- theta["mu", ]
log_scales <- theta["phi", ]
shapes <- theta["gamma", ]

length(weights[weights > 0.001])

final_models <- lapply(1:p, function(k){
  model <- maxLik(logLik = loglik_em, 
                  start = c(mu = sum(weights*x), phi = log(sd(weights*x)), gamma = -0.001), 
                  method = c("NR", "BFGS")[1],
                  x = x,
                  weights = posterior[k, ])
  
  model
})


model_1 <- final_models[[1]]

summary(model_1)




#-------------------------------------------------------------------------------


library(mixdist)
data(pikepar)


fitpike1 <- mixdist::mix(mixdat = x, dist = "norm", pikepar)



#-------------------------------------------------------------------------------

u <- bmixture::rdirichlet(n = 1, alpha = c(0.7, 0.2, 0.1))
u






#-------------------------------------------------------------------------------







data = rmixnorm(n = n, weight = weight, mean = mean, sd = sd)


#-------------------------------------------------------------------------------


library(flexmix)




rdirichlet( n = 1, alpha = c( 1, 1, 1 ) )




#-------------------------------------------------------------------------------

library(flexmix)

library(mixR)
library(mixdist)

library(bmixture)

data(galaxy)


set.seed(70)
# Runing bdmcmc algorithm for the galaxy dataset
mcmc_sample = bmixnorm(data = galaxy[,1])

summary(mcmc_sample)
plot(mcmc_sample)
print(mcmc_sample)
# simulating data from mixture of Normal with 3 components
n = 500
weight = c(0.3, 0.5, 0.2)
mean= c(0 , 10 , 3)
sd= c(1 , 1 , 1)
data = rmixnorm(n = n, weight = weight, mean = mean, sd = sd)
# plot for simulation data
hist(data, prob = TRUE, nclass = 30, col = "gray")
x= seq(-20, 20, 0.05)
densmixnorm = dmixnorm(x, weight, mean, sd)
lines(x, densmixnorm, lwd = 2)
# Runing bdmcmc algorithm for the above simulation data set
bmixnorm.obj = bmixnorm(data, k = 3, iter = 1000)
summary(bmixnorm.obj)





#-------------------------------------------------------------------------------











#-------------------------------------------------------------------------------