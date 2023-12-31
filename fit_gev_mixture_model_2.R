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

library(zoo)

#-------------------------------------------------------------------------------

n <- 10000

x <- bmixture::rmixnorm(n = n, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))

#------

density_object <- density(x, n = 1024)

pdf <- density_object$y

support <- density_object$x

plot(support, pdf, type = "l", main = "PDF")

cdf_object <- ecdf(x) 

cdf <- cdf_object(support)

plot(support, cdf, type = "l", main = "CDF")

#------

max_series <- zoo::rollapply(pdf, width = 2, FUN = max)

max_series_diff <- diff(max_series)

max_series_diff_zero_positions <- which(max_series_diff == 0)

pdf_modes <- pdf[max_series_diff_zero_positions]

support_modes <- support[max_series_diff_zero_positions]

#------

min_series <- zoo::rollapply(pdf, width = 2, FUN = min)

min_series_diff <- diff(min_series)

min_series_diff_zero_positions <- which(min_series_diff == 0)

pdf_negative_modes <- pdf[min_series_diff_zero_positions]

support_negative_modes <- support[min_series_diff_zero_positions]

#------

plot(support, pdf, type = "l", main = "PDF")

abline(h = pdf_modes, lty = "dotted")
abline(h = pdf_negative_modes, lty = "dotted")

abline(v = support_modes, lty = "dotted")
abline(v = support_negative_modes, lty = "dotted")


#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------

loglik_em <- function(theta, x, weights){
  mu <- theta[1]
  phi <- theta[2]
  gamma <- theta[3]
  
  sigma <- exp(phi)
  
  terms <- extRemes::devd(x = x, loc = mu, scale = sigma, shape = gamma, log = TRUE, type = "GEV")
  
  sum(weights*terms)
}

#-------------------------------------------------------------------------------

source("./src/calculate_modes.R")

fit_gev_mixture_model_2 <- function(x, 
                                    nclusters = 1, 
                                    min_cluster_size = 10,
                                    max_iteration = 100,
                                    tolerance = 10^(-3)){
  
  x <- x
  n <- length(x)
  p <- nclusters
  
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
  omega <- bmixture::rdirichlet(n = 1, alpha = rep(x = 1, times = p))[1, ]
  
  theta <- sapply(clusters_labels, function(k){
    y <- x[which(z == k)]
    location <- rnorm(n = 1, mean = mean(y), sd = 1)
    log_scale <- log(sd(y))
    shape <- 0
    parameters <- c(location, log_scale, shape)
    names(parameters) <- c("location", "log_scale", "shape")
    parameters
  })
  
  locations <- theta["location", ]
  log_scales <- theta["log_scale", ]
  shapes <- theta["shape", ]
  
  posterior <- sapply(1:n, function(i){
    obs <- x[i]
    likelihood <- sapply(1:p, function(k){
      location <- locations[k]
      scale <- exp(log_scales[k])
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
  
  negative_loglik <- sapply(clusters_labels, function(k){
    if (class(posterior)[1] == "numeric"){
      weights <- as.numeric(posterior)
    } else{
      weights <- posterior[k, ]
    }
    
    y <- x[which(z == k)]
    model <- maxLik(logLik = loglik_em, 
                    start = c(mu = locations[k], phi = log_scales[k], gamma = shapes[k]), 
                    method = c("NR", "BFGS", "SANN")[3],
                    x = x,
                    weights = weights)
    -as.numeric(logLik(model))
  })
  
  current_negative_loglik <- sum(negative_loglik)
  
  current_tolerance <- tolerance + 1
  current_iteration <- 1
  
  while (current_iteration < max_iteration & current_tolerance > tolerance){
    current_iteration <- current_iteration + 1
    
    if (class(posterior)[1] == "numeric"){
      omega <- as.numeric(mean(posterior))
    } else{
      omega <- apply(posterior, 1, mean)
    }
    
    if (class(posterior)[1] == "numeric"){
      cluster_sizes <- sum(as.numeric(posterior))
    } else{
      cluster_sizes <- apply(posterior, 1, sum)
    }
    
    if (p > 1){
      if (min(cluster_sizes) < min_cluster_size){
        p <- p - 1
        
        modes_object <- calculate_modes(x = x)
        density_minima <- modes_object$density_minima 
        density_minima_selected <- tail(sort(density_minima), n = p - 1)
        breaks = modes_object$density_minima_argument
        breaks <- c(min(x), breaks[density_minima %in% density_minima_selected], max(x))
        z <- cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
        
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
      if (class(posterior)[1] == "numeric"){
        weights <- as.numeric(posterior)
      } else{
        weights <- posterior[k, ]
      }
      
      y <- x[which(z == k)]
      model <- maxLik(logLik = loglik_em, 
                      start = c(mu = locations[k], phi = log_scales[k], gamma = shapes[k]), 
                      method = c("NR", "BFGS", "SANN")[3],
                      x = x,
                      weights = weights)
      coefficients(model)
    })
    
    locations <- theta["mu", ]
    log_scales <- theta["phi", ]
    shapes <- theta["gamma", ]
    
    posterior <- sapply(1:n, function(i){
      obs <- x[i]
      likelihood <- sapply(1:p, function(k){
        location <- locations[k]
        scale <- exp(log_scales[k])
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
    
    negative_loglik <- sapply(clusters_labels, function(k){
      if (class(posterior)[1] == "numeric"){
        weights <- as.numeric(posterior)
      } else{
        weights <- posterior[k, ]
      }
      
      y <- x[which(z == k)]
      model <- maxLik(logLik = loglik_em, 
                      start = c(mu = locations[k], phi = log_scales[k], gamma = shapes[k]), 
                      method = c("NR", "BFGS", "SANN")[3],
                      x = x,
                      weights = weights)
      -as.numeric(logLik(model))
    })
    
    current_tolerance <- abs(current_negative_loglik - sum(negative_loglik))
    current_negative_loglik <- sum(negative_loglik)
    
    print(paste0("Iteration: ", current_iteration, ", Tolerance: ", current_tolerance))
  }
  
  # print("The total number of iterations is equal to ", current_iteration)
  # print("The variation of last consecutive negative loglikelihoods is equal to ", current_tolerance)
  
  final_models <- lapply(clusters_labels, function(k){
    if (class(posterior)[1] == "numeric"){
      weights <- as.numeric(posterior)
    } else{
      weights <- posterior[k, ]
    }
    
    y <- x[which(z == k)]
    model <- maxLik(logLik = loglik_em, 
                    start = c(mu = locations[k], phi = log_scales[k], gamma = shapes[k]), 
                    method = c("NR", "BFGS", "SANN")[3],
                    x = x,
                    weights = weights)
    model
  })
  
  
  list(current_iteration = current_iteration,
       current_tolerance = current_tolerance,
       cluster_weights = omega,
       clusters = z,
       cluster_models = final_models)

}




#-------------------------------------------------------------------------------

source("./src/calculate_gev_mixture_model_pdf.R")
source("./src/generate_gev_mixture_model_sample.R")

weights <- c(0.5, 0.5)

shapes <- c(0.1, 0.1)
scales <- c(1, 1)
locations <- c(-2, +2)



weights <- c(0.5, 0.5, 0.5)

shapes <- c(0.1, 0.1, 0.1)
scales <- c(1, 1, 1)
locations <- c(-2, +2, +6)

n <- 2000

x <- generate_gev_mixture_model_sample(n = n,
                                       locations,
                                       scales,
                                       shapes,
                                       weights,
                                       kind = c("geometric", "arithmetic")[2])

# x <- extRemes::revd(n = n, loc = 1, scale = 1, shape = 0.2, type = "GEV")

# x <- bmixture::rmixnorm(n = n, weight = c(0.5, 0.5), mean = c(-5, 0), sd = c(1, 1))

p <- 2

results <- fit_gev_mixture_model_2(x, 
                                   nclusters = p, 
                                   min_cluster_size = 20, 
                                   max_iteration = 40,
                                   tolerance = 10^(-3))

results

#-------------------------------------------------------------------------------

# plot 1 component

support <- sort(x)

model_1 <- results$cluster_models[[1]]
par_model_1 <- coefficients(model_1)
dens_1 <- devd(x = support, loc = par_model_1[1], scale = exp(par_model_1[2]), shape = par_model_1[3])

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
par_model_1 <- coefficients(model_1)
dens_1 <- devd(x = support, loc = par_model_1[1], scale = exp(par_model_1[2]), shape = par_model_1[3])



model_2 <- results$cluster_models[[2]]
par_model_2 <- coefficients(model_2)
dens_2 <- devd(x = support, loc = par_model_2[1], scale = exp(par_model_2[2]), shape = par_model_2[3])


parameters <- rbind(par_model_1, par_model_2)
weights <- results$cluster_weights

pdf_1 <- calculate_gev_mixture_model_pdf(x = support,
                                       locations = parameters[, 1],
                                       scales =  exp(parameters[, 2]),
                                       shapes = parameters[, 3],
                                       weights = weights,
                                       kind = c("geometric", "arithmetic")[1])


pdf_2 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations = parameters[, 1],
                                         scales =  exp(parameters[, 2]),
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
       lty = c(1, 2, 2, 2, 1, 1), col = c(1, 3, 4, 5, 7, 6))


#-------------------------------------------------------------------------------

# plot 3 components

support <- sort(x)

model_1 <- results$cluster_models[[1]]
par_model_1 <- coefficients(model_1)
dens_1 <- devd(x = support, loc = par_model_1[1], scale = exp(par_model_1[2]), shape = par_model_1[3])



model_2 <- results$cluster_models[[2]]
par_model_2 <- coefficients(model_2)
dens_2 <- devd(x = support, loc = par_model_2[1], scale = exp(par_model_2[2]), shape = par_model_2[3])


model_3 <- results$cluster_models[[3]]
par_model_3 <- coefficients(model_3)
dens_3 <- devd(x = support, loc = par_model_3[1], scale = exp(par_model_3[2]), shape = par_model_3[3])


parameters <- rbind(par_model_1, par_model_2, par_model_3)
weights <- results$cluster_weights

pdf_1 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations = parameters[, 1],
                                         scales =  exp(parameters[, 2]),
                                         shapes = parameters[, 3],
                                         weights = weights,
                                         kind = c("geometric", "arithmetic")[1])


pdf_2 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations = parameters[, 1],
                                         scales =  exp(parameters[, 2]),
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
print(paste0("Iteration: ", current_iteration, ", Negative loglikelihood: ", current_negative_loglik))










#-------------------------------------------------------------------------------