#' ---
#' title: "Modeling extreme values with a single GEV probability distribution"
#' author: "Pascal Alain Dkengne Sielenou"
#' date: "September 08th, 2023"
#' output: pdf_document
#' ---


#'
source("./src/extract_block_maxima_with_indexes.R")
source("./src/estimate_single_gev_model.R")
source("./src/generate_gev_sample.R")
source("./src/plot_gev_pdf.R")
source("./src/plot_gev_cdf.R")
source("./src/plot_gev_probability.R")
source("./src/plot_gev_quantile.R")
source("./src/plot_block_maxima.R")

#'
x <- rnorm(n = 1000)

#'
block_size <- 40

#'
extremes <- extract_block_maxima_with_indexes(x, block_size)

extremes

#'
plot_block_maxima(x, block_size, xlab = "Index", ylab = "Values", main = "Block maxima")

#'
model <- estimate_single_gev_model(x, block_size, nsloc = NULL)

#'
names(model)

#'
model$gev_model

#'
names(model$gev_model)

#'
model$normalized_gev_parameters

#'
plot_gev_pdf(model, 
             zoom = FALSE,
             xlab = "Quantile", 
             ylab = "Density", 
             main = "Probability Density Function (PDF) Plot")

#'
plot_gev_pdf(model, 
             zoom = TRUE,
             xlab = "Quantile", 
             ylab = "Density", 
             main = "Probability Density Function (PDF) Plot")

#'
plot_gev_cdf(model,
             zoom = FALSE,
             xlab = "Quantile", 
             ylab = "Cumulative Probability",
             main = "Cumulative Distribution Function (CDF) Plot")

#'
plot_gev_cdf(model,
             zoom = TRUE,
             xlab = "Quantile", 
             ylab = "Cumulative Probability",
             main = "Cumulative Distribution Function (CDF) Plot")

#'
plot_gev_probability(model, 
                     xlab = "Theoretical Probability", 
                     ylab = "Empirical Probability", 
                     main = "Probability Plot")


#'
plot_gev_quantile(model, 
                  xlab = "Theoretical Quantile", 
                  ylab = "Empirical Quantile", 
                  main = "Quantile Plot")





