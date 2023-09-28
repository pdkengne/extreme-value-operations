#' ---
#' title: "Modeling extreme values with a GEV mixture probability distributions"
#' author: "Pascal Alain Dkengne Sielenou"
#' date: "September 28th, 2023"
#' output: pdf_document
#' ---

#'
# library(xfun)

#'
path <- ".."

#'
xfun::in_dir(dir = path, expr = source("./src/generate_gev_sample.R"))
xfun::in_dir(dir = path, expr = source("./src/calculate_gev_inverse_cdf.R"))
xfun::in_dir(dir = path, expr = source("./src/estimate_gev_mixture_model_parameters.R"))
xfun::in_dir(dir = path, expr = source("./src/plot_gev_mixture_model_pdf.R"))
xfun::in_dir(dir = path, expr = source("./src/plot_several_standardized_block_maxima_mean.R"))
xfun::in_dir(dir = path, expr = source("./src/estimate_gev_mixture_model_quantile.R"))

#'
n <- 20000

#'
set.seed(1122)
x <- rnorm(n = n)

#+ fig.width=12, fig.height=8
hist(x)

#+ fig.width=12, fig.height=8
acf(x)

#'
nlargest <- 1000

#
y <- extract_nlargest_sample(x, n = nlargest)

#'
gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
                                                           nsloc = NULL,
                                                           std.err = FALSE,
                                                           block_sizes = NULL,
                                                           minimum_nblocks = 50,
                                                           threshold = NULL,
                                                           nlargest = nlargest,
                                                           confidence_level = 0.95,
                                                           log_mv = TRUE,
                                                           log_pw = TRUE,
                                                           trace = FALSE)

#'
names(gev_mixture_model)

#'
gev_mixture_model$block_sizes

#'
gev_mixture_model$normalized_gev_parameters_object

#'
gev_mixture_model$weighted_normalized_gev_parameters_object

#'
gev_mixture_model$automatic_weights_mw_statistics

#'
gev_mixture_model$automatic_weights_pw_statistics

#'
gev_mixture_model$automatic_weights_mw

#'
gev_mixture_model$pessimistic_weights_pw_shape

#'
gev_mixture_model$pessimistic_weights_pw_scale

#'
gev_mixture_model$pessimistic_weights_pw_loc

#+ fig.width=12, fig.height=8
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = FALSE,
                           zoom = FALSE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

#+ fig.width=12, fig.height=8
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = FALSE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

#+ fig.width=12, fig.height=8
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = FALSE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

#+ fig.width=12, fig.height=8
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

#'
estimator_types <- c("automatic_weights_mw",
                     "pessimistic_weights_mw",
                     "identic_weights_mw",
                     "automatic_weights_pw",
                     "pessimistic_weights_pw",
                     "identic_weights_pw",
                     "empirical",
                     "confidence_interval_mw",
                     "confidence_interval_pw")

#'
alpha <- 10^(-14)

#'
rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[1])

rl_mw

#'
rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[4])

rl_pw

#'
rl_empirical <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                    alpha = alpha,
                                                    confidence_level = 0.95,
                                                    do.ci = TRUE,
                                                    estimator_type = estimator_types[7])

rl_empirical

#'
true_rl <- qnorm(p = 1 - alpha)
true_rl

#'
est_rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                 alpha = alpha,
                                                 confidence_level = 0.95,
                                                 do.ci = TRUE,
                                                 estimator_type = estimator_types[9])

est_rl_pw

#'
est_rl_pw_range <- range(as.matrix(est_rl_pw))
est_rl_pw_range

#'
est_rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                 alpha = alpha,
                                                 confidence_level = 0.95,
                                                 do.ci = TRUE,
                                                 estimator_type = estimator_types[8])

est_rl_mw

#'
est_rl_mw_range <- range(as.matrix(est_rl_mw))
est_rl_mw_range

#+ fig.width=12, fig.height=8
matplot(x = rownames(est_rl_pw), 
        y = est_rl_pw, 
        xlab = "block size",
        ylab = "quantile",
        main = "Estimates of a quantile",
        ylim = range(c(est_rl_pw_range, true_rl)),
        cex = 1,
        cex.lab = 1,
        cex.axis = 1,
        type = "l", 
        lty = c("dotted", "solid", "dotted"), 
        lwd = c(2,2,2), 
        col = c(3, 1, 3))

abline(h = true_rl, col = 4, lwd = 2)
abline(h = rl_mw[2], col = 7, lwd = 2)
abline(h = rl_pw[2], col = 6, lwd = 2)
abline(h = est_rl_pw_range, col = 6, lty = "dotted", lwd = 2)
abline(h = est_rl_mw_range, col = 7, lty = "dotted", lwd = 2)
