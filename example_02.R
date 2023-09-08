#' ---
#' title: "Modeling extreme values with a GEV mixture probability distributions"
#' author: "Pascal Alain Dkengne Sielenou"
#' date: "September 08th, 2023"
#' output: html_document
#' ---


#'
source("./src/estimate_gev_mixture_model_parameters.R")
source("./src/plot_gev_mixture_model_pdf.R")
source("./src/generate_gev_sample.R")
source("./src/plot_normalized_gev_mixture_model_pdf.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_inverse_cdf.R")
source("./src/calculate_gev_mixture_model_cdf.R")


#'
n <- 10000

nlargest <- 1000

# x <- generate_gev_sample(n = n, loc = 1, scale = 0.5, shape = 0.1)
x <- rnorm(n = n)

gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
                                                           nsloc = NULL,
                                                           std.err = FALSE,
                                                           block_sizes = NULL,
                                                           minimum_nblocks = 50,
                                                           nlargest = nlargest,
                                                           confidence_level = 0.95,
                                                           trace = TRUE)
#'
names(gev_mixture_model)

#'
gev_mixture_model$block_sizes

#'
gev_mixture_model$normalized_gev_parameters_object

#'
gev_mixture_model$automatic_weights_mw

#'
gev_mixture_model$automatic_weights_mw_statistics

#'
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")


#'
gev_mixture_model_parameters <- gev_mixture_model$normalized_gev_parameters_object

shapes <- gev_mixture_model_parameters$shape_star
scales <- gev_mixture_model_parameters$scale_star
locations <- gev_mixture_model_parameters$loc_star

weights <- gev_mixture_model$automatic_weights_mw


#'
p <- seq(from = 0.90, to = 0.99, length.out = 10)
p

#'
quantiles <- calculate_gev_mixture_model_inverse_cdf(p = p, locations, scales, shapes, weights, iterations = 100)

quantiles

#'
probaility <- calculate_gev_mixture_model_cdf(q = quantiles, locations, scales, shapes, weights)

probaility

#'
qnorm(p = p)

#'
calculate_gev_inverse_cdf(p = p, loc = 1, scale = 0.5, shape = 0.1)
