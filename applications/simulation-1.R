setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

library(tidyverse)
library(DescTools)

source("./src/generate_gev_mixture_model_sample.R")
source("./src/calculate_gev_mixture_model_pdf.R")

source("./src/estimate_gev_mixture_model_parameters.R")
source("./src/estimate_gev_mixture_model_quantile.R")

source("./src/calculate_gev_mixture_model_inverse_cdf.R")

source("./src/plot_gev_mixture_model_pdf.R")
source("./src/plot_gev_mixture_model_cdf.R")


# example 1

weights <- c(0.5, 0.5)

shapes <- c(0.1, 0.1)
scales <- c(1, 1)
locations <- c(-2, -1)

n <- 20000

sample_1 <- generate_gev_mixture_model_sample(n = n,
                                              locations,
                                              scales,
                                              shapes,
                                              weights,
                                              kind = c("geometric", "arithmetic")[2])

support <- sort(sample_1)

pdf_1 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations,
                                         scales,
                                         shapes,
                                         weights,
                                         kind = c("geometric", "arithmetic")[2])

plot(support, pdf_1, type = "l", 
     lwd = 2, xlab = "quantile", ylab = "density", 
     main = "density plot")


x <- sample_1

Desc(x)

nlargest <- 1000
y <- extract_nlargest_sample(x, n = nlargest)

Desc(y)


gev_mixture_model <- suppressWarnings(estimate_gev_mixture_model_parameters(x = x,
                                                                            kind = c("geometric", "arithmetic")[1],
                                                                            block_sizes = NULL,
                                                                            minimum_nblocks = 50,
                                                                            threshold = NULL,
                                                                            nlargest = nlargest,
                                                                            confidence_level = 0.95,
                                                                            use_extremal_index = TRUE,
                                                                            use_lower_threshold = FALSE,
                                                                            maximum_iterations = 1500,
                                                                            log_mv = TRUE,
                                                                            log_pw = TRUE,
                                                                            trace = TRUE,
                                                                            method = "MLE"))

print(gev_mixture_model$automatic_weights_mw)

print(gev_mixture_model$extremal_indexes)

#print(gev_mixture_model$normalized_gev_parameters_object)

print(gev_mixture_model$full_normalized_gev_parameters_object)

plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

plot_gev_mixture_model_cdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Cumulative Probability",
                           main = "Cumulative Distribution Function (CDF) Plot")

estimator_types <- c("automatic_weights_mw", 
                     "pessimistic_weights_mw", 
                     "identic_weights_mw", 
                     "automatic_weights_pw",
                     "pessimistic_weights_pw", 
                     "identic_weights_pw", 
                     "model_wise",
                     "parameter_wise",
                     "empirical")

alpha <- 10^(-6)

rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[1])

rl_mw

est_rl_pw <- suppressWarnings(estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                                  alpha = alpha,
                                                                  confidence_level = 0.95,
                                                                  do.ci = TRUE,
                                                                  estimator_type = estimator_types[8]))
est_rl_pw


true_rl <-  calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha,
                                                    locations,
                                                    scales,
                                                    shapes,
                                                    weights,
                                                    iterations = 100,
                                                    kind = c("geometric", "arithmetic")[2])

true_rl


matplot(x = rownames(est_rl_pw), 
        y = est_rl_pw, 
        xlab = "block size",
        ylab = "quantile",
        main = "Estimates of a quantile",
        ylim = range(est_rl_pw),
        cex = 1,
        cex.lab = 1,
        cex.axis = 1,
        type = "l", 
        lty = c("dotted", "solid", "dotted"), 
        lwd = c(2,2,2), 
        col = c(3, 1, 3))

abline(h = true_rl, col = 4, lwd = 2)
abline(h = rl_mw, col = 6, lwd = 2)
abline(h = range(est_rl_pw), col = 6, lty = "dotted", lwd = 2)


#-------------------------------------------------------------------------------
# example 2

weights <- c(0.5, 0.5)

shapes <- c(0.1, 0.1)
scales <- c(1, 1)
locations <- c(-2, +2)

n <- 20000

sample_1 <- generate_gev_mixture_model_sample(n = n,
                                              locations,
                                              scales,
                                              shapes,
                                              weights,
                                              kind = c("geometric", "arithmetic")[2])

support <- sort(sample_1)

pdf_1 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations,
                                         scales,
                                         shapes,
                                         weights,
                                         kind = c("geometric", "arithmetic")[2])

plot(support, pdf_1, type = "l", 
     lwd = 2, xlab = "quantile", ylab = "density", 
     main = "density plot")


x <- sample_1

Desc(x)

nlargest <- 1000
y <- extract_nlargest_sample(x, n = nlargest)

Desc(y)


gev_mixture_model <- suppressWarnings(estimate_gev_mixture_model_parameters(x = x,
                                                                            kind = c("geometric", "arithmetic")[1],
                                                                            block_sizes = NULL,
                                                                            minimum_nblocks = 50,
                                                                            threshold = NULL,
                                                                            nlargest = nlargest,
                                                                            confidence_level = 0.95,
                                                                            use_extremal_index = TRUE,
                                                                            use_lower_threshold = FALSE,
                                                                            maximum_iterations = 1500,
                                                                            log_mv = TRUE,
                                                                            log_pw = TRUE,
                                                                            trace = TRUE,
                                                                            method = "MLE"))

print(gev_mixture_model$automatic_weights_mw)

print(gev_mixture_model$extremal_indexes)

#print(gev_mixture_model$normalized_gev_parameters_object)

print(gev_mixture_model$full_normalized_gev_parameters_object)

plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

plot_gev_mixture_model_cdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Cumulative Probability",
                           main = "Cumulative Distribution Function (CDF) Plot")

estimator_types <- c("automatic_weights_mw", 
                     "pessimistic_weights_mw", 
                     "identic_weights_mw", 
                     "automatic_weights_pw",
                     "pessimistic_weights_pw", 
                     "identic_weights_pw", 
                     "model_wise",
                     "parameter_wise",
                     "empirical")

alpha <- 10^(-6)

rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[1])

rl_mw

est_rl_pw <- suppressWarnings(estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                                  alpha = alpha,
                                                                  confidence_level = 0.95,
                                                                  do.ci = TRUE,
                                                                  estimator_type = estimator_types[8]))
est_rl_pw


true_rl <-  calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha,
                                                    locations,
                                                    scales,
                                                    shapes,
                                                    weights,
                                                    iterations = 100,
                                                    kind = c("geometric", "arithmetic")[2])

true_rl


matplot(x = rownames(est_rl_pw), 
        y = est_rl_pw, 
        xlab = "block size",
        ylab = "quantile",
        main = "Estimates of a quantile",
        ylim = range(est_rl_pw),
        cex = 1,
        cex.lab = 1,
        cex.axis = 1,
        type = "l", 
        lty = c("dotted", "solid", "dotted"), 
        lwd = c(2,2,2), 
        col = c(3, 1, 3))

abline(h = true_rl, col = 4, lwd = 2)
abline(h = rl_mw, col = 6, lwd = 2)
abline(h = range(est_rl_pw), col = 6, lty = "dotted", lwd = 2)



#-------------------------------------------------------------------------------
# example 3

weights <- c(0.5, 0.5, 0.5)

shapes <- c(0.1, 0.1, 0.1)
scales <- c(1, 1, 1)
locations <- c(-2, +2, +6)

n <- 20000

sample_1 <- generate_gev_mixture_model_sample(n = n,
                                              locations,
                                              scales,
                                              shapes,
                                              weights,
                                              kind = c("geometric", "arithmetic")[2])

support <- sort(sample_1)

pdf_1 <- calculate_gev_mixture_model_pdf(x = support,
                                         locations,
                                         scales,
                                         shapes,
                                         weights,
                                         kind = c("geometric", "arithmetic")[2])

plot(support, pdf_1, type = "l", 
     lwd = 2, xlab = "quantile", ylab = "density", 
     main = "density plot")


x <- sample_1

Desc(x)

nlargest <- 1000
y <- extract_nlargest_sample(x, n = nlargest)

Desc(y)


gev_mixture_model <- suppressWarnings(estimate_gev_mixture_model_parameters(x = x,
                                                                            kind = c("geometric", "arithmetic")[1],
                                                                            block_sizes = NULL,
                                                                            minimum_nblocks = 50,
                                                                            threshold = NULL,
                                                                            nlargest = nlargest,
                                                                            confidence_level = 0.95,
                                                                            use_extremal_index = TRUE,
                                                                            use_lower_threshold = FALSE,
                                                                            maximum_iterations = 1500,
                                                                            log_mv = TRUE,
                                                                            log_pw = TRUE,
                                                                            trace = TRUE,
                                                                            method = "MLE"))

print(gev_mixture_model$automatic_weights_mw)

print(gev_mixture_model$extremal_indexes)

#print(gev_mixture_model$normalized_gev_parameters_object)

print(gev_mixture_model$full_normalized_gev_parameters_object)

plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

plot_gev_mixture_model_cdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Cumulative Probability",
                           main = "Cumulative Distribution Function (CDF) Plot")

estimator_types <- c("automatic_weights_mw", 
                     "pessimistic_weights_mw", 
                     "identic_weights_mw", 
                     "automatic_weights_pw",
                     "pessimistic_weights_pw", 
                     "identic_weights_pw", 
                     "model_wise",
                     "parameter_wise",
                     "empirical")

alpha <- 10^(-6)

rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[1])

rl_mw

est_rl_pw <- suppressWarnings(estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                                  alpha = alpha,
                                                                  confidence_level = 0.95,
                                                                  do.ci = TRUE,
                                                                  estimator_type = estimator_types[8]))
est_rl_pw


true_rl <-  calculate_gev_mixture_model_inverse_cdf(p = 1 - alpha,
                                                    locations,
                                                    scales,
                                                    shapes,
                                                    weights,
                                                    iterations = 100,
                                                    kind = c("geometric", "arithmetic")[2])

true_rl


matplot(x = rownames(est_rl_pw), 
        y = est_rl_pw, 
        xlab = "block size",
        ylab = "quantile",
        main = "Estimates of a quantile",
        ylim = range(est_rl_pw),
        cex = 1,
        cex.lab = 1,
        cex.axis = 1,
        type = "l", 
        lty = c("dotted", "solid", "dotted"), 
        lwd = c(2,2,2), 
        col = c(3, 1, 3))

abline(h = true_rl, col = 4, lwd = 2)
abline(h = rl_mw, col = 6, lwd = 2)
abline(h = range(est_rl_pw), col = 6, lty = "dotted", lwd = 2)

