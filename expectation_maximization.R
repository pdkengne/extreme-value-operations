#-------------------------------------------------------------------------------
setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")
#-------------------------------------------------------------------------------

library(tidyverse)

library(DescTools)

library(maxLik)

library(extRemes)

library(bmixture)

#-------------------------------------------------------------------------------














#-------------------------------------------------------------------------------

loglik <- function(theta, x){
  mu <- theta[1]
  phi <- theta[2]
  gamma <- theta[3]
  
  sigma <- exp(phi)
  
  x_0 <- rep(x = 1, times = length(x))
  
  terms <- -x_0*log(sigma) - (1 + 1/gamma)*log(1 + gamma*(x - mu)/sigma) - (1 + gamma*(x - mu)/sigma)^(-1/gamma)
  
  sum(terms)
}



x <- extRemes::revd(n = 1000, loc = 1, scale = 1, shape = 0.2, type = "GEV")

m <- maxLik(logLik = loglik, 
            start = c(mu = mean(x), phi = log(sd(x)), gamma = -0.001), 
            method = "NR",
            x = x)

summary(m)

#-------------------------------------------------------------------------------



loglik <- function(theta, x){
  mu <- theta[1]
  phi <- theta[2]
  gamma <- theta[3]
  
  sigma <- exp(phi)
  
  terms <- extRemes::devd(x = x, loc = mu, scale = sigma, shape = gamma, log = TRUE, type = "GEV")
  
  sum(terms)
}


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