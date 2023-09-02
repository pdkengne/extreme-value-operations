source("./src/calculate_gev_pdf.R")
source("./src/calculate_gev_cdf.R")
source("./src/calculate_gev_mixture_model_cdf.R")

calculate_gev_mixture_model_pdf <- function(x, locations, scales, shapes, weights){
  # x: vector of observations
  # weights: vector of weights
  # locations, scales, shapes: vectors of location, scale and shape parameters of the considered gev distributions
  # The vectors of parameters must have the same number of elements
  
  S <- sapply(1:length(weights), function(j) {
    dens <- calculate_gev_pdf(x = x, 
                              loc = locations[j], 
                              scale = scales[j], 
                              shape = shapes[j])
    
    prob <- calculate_gev_mixture_model_cdf(q = x, 
                                            locations = locations[-j], 
                                            scales = scales[-j], 
                                            shapes = shapes[-j], 
                                            weights = weights[-j])
    
    out <- weights[j]*dens*prob
    
    out
  })
    
  g <- sum(S)
  
  g
}


# example 1

p <- 10

y <- runif(p)
weights <- y/sum(y)

shapes <- runif(n = p, min = -0.5, max = +0.5)
scales <- rexp(n = p)
locations <- rnorm(n = p)

results <- calculate_gev_mixture_model_pdf(x = 2, locations, scales, shapes, weights)

results


# example 2

p <- 100

y <- runif(p)
weights <- y/sum(y)

shapes <- runif(n = p, min = -0.5, max = +0.5)
scales <- rexp(n = p)
locations <- rnorm(n = p)

results <- calculate_gev_mixture_model_pdf(x = 10, locations, scales, shapes, weights)

results
















