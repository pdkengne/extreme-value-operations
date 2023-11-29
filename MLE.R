setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

library(tidyverse)

library(DescTools)

require(numDeriv)

library(BB)

library(maxLik)


#-------------------------------------------------------------------------------

source <- "./applications/final_dataset.csv"
data <-  read.csv(file = source, sep = ",")


#-------------------------------------------------------------------------------

poissmix.loglik <- function(p,y) {
  # Log-likelihood for a binary Poisson mixture distribution
  i <- 0:(length(y)-1)
  loglik <- y * log(p[1] * exp(-p[2]) * p[2]^i / exp(lgamma(i+1)) +
                      (1 - p[1]) * exp(-p[3]) * p[3]^i / exp(lgamma(i+1)))
  return (sum(loglik) )
}


poissmix.dat <- data.frame(death=0:9, freq=c(162,267,271,185,111,61,27,8,3,1))

lo <- c(0,0,0) # lower limits for parameters

hi <- c(1, Inf, Inf) # upper limits for parameters

p0 <- runif(3,c(0.2,1,1),c(0.8,5,8)) # a randomly generated vector of length 3

y <- c(162,267,271,185,111,61,27,8,3,1)

ans1 <- spg(par=p0, fn=poissmix.loglik, y=y,
            lower=lo, upper=hi, control=list(maximize=TRUE, trace=TRUE))

ans1

ans2 <- BBoptim(par=p0, fn=poissmix.loglik, y=y,
                lower=lo, upper=hi, control=list(maximize=TRUE))

ans2


hess <- hessian(x=ans2$par, func=poissmix.loglik, y=y)

hess

se <- sqrt(diag(solve(-hess)))

se












#-------------------------------------------------------------------------------

x <- rnorm(1000) # data. true mu = 0, sigma = 1

loglik <- function(theta, x){
    mu <- theta[1]
    sigma <- theta[2]
    
    sum(dnorm(x, mean=mu, sd=sigma, log=TRUE))
}

m <- maxLik(loglik, start=c(mu=1, sigma=2), x=x)

m

# give start value somewhat off
s <- summary(m)

s


names(m)

m$estimate

coef(m)

AIC(m)

vcov(m)

stdEr(m)





#-------------------------------------------------------------------------------

data(CO2)

CO2

loglik <- function(theta) {
    mu <- theta[1]
    sigma <- theta[2]
    N <- nrow(CO2)
    
    -N*log(sqrt(2*pi)) - N*log(sigma) -
    0.5*sum((CO2$uptake - mu)^2/sigma^2)
}

m <- maxLik(loglik, start=c(mu=30, sigma=10))

summary(m)


#-------------------------------------------------------------------------------


loglik <- function(theta) {
  beta0 <- theta[1]
  beta1 <- theta[2]
  sigma <- theta[3]
 
  N <- nrow(CO2)  # compute new mu based on beta1, beta2
 mu <- beta0 + beta1*CO2$conc # use this mu in a similar fashion as previously
 
 -N*log(sqrt(2*pi)) - N*log(sigma) -
 0.5*sum((CO2$uptake - mu)^2/sigma^2)
}

m <- maxLik(loglik, start=c(beta0=30, beta1=0, sigma=10), method = "BFGS")

summary(m)




#-------------------------------------------------------------------------------


loglik <- function(theta) {
  beta0 <- theta[1]
  beta1 <- theta[2]
  sigma <- theta[3]
  
  N <- nrow(CO2)  # compute new mu based on beta1, beta2
  mu <- beta0 + beta1*CO2$conc # use this mu in a similar fashion as previously
  
  sum(log(dnorm(x, mean=mu, sd=sigma)))
}

m <- maxLik(loglik, start=c(beta0=30, beta1=0, sigma=10), method = "BFGS")

summary(m)

ans2 <- BBoptim(par=c(beta0=30, beta1=0, sigma=10), fn=loglik, control=list(maximize=TRUE))

ans2


#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------





