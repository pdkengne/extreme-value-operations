#' ---
#' title: "Modeling extreme values with a GEV mixture probability distributions"
#' subtitle: "Application to a rain data in australia"
#' author: "Pascal Alain Dkengne Sielenou"
#' date: "`r Sys.Date()`"
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
xfun::in_dir(dir = path, expr = source("./src/predict_gev_mixture_model_parameters.R"))
xfun::in_dir(dir = path, expr = source("./src/plot_gev_mixture_model_pdf.R"))
xfun::in_dir(dir = path, expr = source("./src/plot_gev_mixture_model_cdf.R"))
xfun::in_dir(dir = path, expr = source("./src/estimate_gev_mixture_model_quantile.R"))
xfun::in_dir(dir = path, expr = source("./src/estimate_gev_mixture_model_pdf.R"))
xfun::in_dir(dir = path, expr = source("./src/estimate_gev_mixture_model_cdf.R"))
xfun::in_dir(dir = path, expr = source("./src/estimate_gev_mixture_model_sample.R"))

#'
library(readr)
library(tidyverse)
library(DataExplorer)
#library(tibble)
#library(explore)


#'
weatherAUS <- xfun::in_dir(dir = path, expr = read_csv("./applications/weatherAUS.csv"))

# View(weatherAUS)

#'
str(weatherAUS)

#'
names(weatherAUS)

#' 
head(weatherAUS)

#'
tail(weatherAUS)

# plot_str(weatherAUS)

# introduce(weatherAUS)

plot_intro(weatherAUS)

plot_missing(weatherAUS)

#'
final_data <- drop_columns(weatherAUS, c("RISK_MM", "Cloud9am", "Cloud3pm", "Evaporation", "Sunshine"))

# names(final_data)

profile_missing(final_data)

#'
plot_bar(final_data)

plot_bar(final_data, by = "RainTomorrow")

#'
plot_histogram(final_data)


plot_correlation(na.omit(final_data), maxcat = 5L)

# plot_correlation(na.omit(final_data), type = "c")

plot_scatterplot(final_data[, c("MinTemp", "MaxTemp", "Rainfall",  "WindGustSpeed", "WindSpeed9am",  "WindSpeed3pm",  "Humidity9am",  
                                "Humidity3pm",   "Pressure9am",   "Pressure3pm",   "Temp9am", "Temp3pm")], 
                 by = "Rainfall",
                 sampled_rows = 3000L)


#'
final_data_clean <- na.omit(final_data)

summary(final_data_clean)


#'
x <- final_data_clean$Rainfall
n <- length(x)

n

#'
nlargest_1 <- 20000
x <- extract_nlargest_sample(x, n = nlargest_1)
n <- length(x)

n


# Histogram of all data
#+ fig.width=12, fig.height=8
hist(x, prob = TRUE)
lines(density(x),
      lwd = 2,
      col = 4)

# Autocorrelation function of all data
#+ fig.width=12, fig.height=8
acf(x)


# Histogram of the largest data
#'
nlargest <- 2000
y <- extract_nlargest_sample(x, n = nlargest)
hist(y, prob = TRUE)
lines(density(y),
      lwd = 2,
      col = 4)

# Autocorrelation function of the largest data
#+ fig.width=12, fig.height=8
acf(y)


# Estimation of gev mixture models
#'
gev_mixture_model <- estimate_gev_mixture_model_parameters(x = x, 
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
                                                           method = "MLE")

#'
gev_mixture_model$extremal_indexes

#'
gev_mixture_model$normalized_gev_parameters_object

#'
gev_mixture_model$full_normalized_gev_parameters_object

#'
gev_mixture_model$automatic_weights_pw_shape

#'
gev_mixture_model$automatic_weights_pw_scale

#'
gev_mixture_model$automatic_weights_pw_loc

#'
gev_mixture_model$weighted_normalized_gev_parameters_object[3, ]

#'
gev_mixture_model$automatic_weights_mw



# Model diagnostics

## GEV mixture model with respect to parameters
#+ fig.width=12, fig.height=16
par(mfrow = c(2, 1))
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = FALSE,
                           zoom = FALSE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = FALSE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")


#+ fig.width=12, fig.height=16
par(mfrow = c(2, 1))
plot_gev_mixture_model_cdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = FALSE,
                           zoom = FALSE,
                           xlab = "Quantile",
                           ylab = "Cumulative Probability",
                           main = "Cumulative Distribution Function (CDF) Plot")

plot_gev_mixture_model_cdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = FALSE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Cumulative Probability",
                           main = "Cumulative Distribution Function (CDF) Plot")


## GEV mixture model with respect to distribution functions
#+ fig.width=12, fig.height=16
par(mfrow = c(2, 1))
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = FALSE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")


#+ fig.width=12, fig.height=16
par(mfrow = c(2, 1))
plot_gev_mixture_model_cdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = FALSE,
                           xlab = "Quantile",
                           ylab = "Cumulative Probability",
                           main = "Cumulative Distribution Function (CDF) Plot")

plot_gev_mixture_model_cdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Cumulative Probability",
                           main = "Cumulative Distribution Function (CDF) Plot")


# Estimation of an extreme quantile
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
alpha <- 10^(-6)

## Quantile from GEV mixture model with respect to parameters
#'
rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[4])

rl_pw[2]


## Quantile from GEV mixture model with respect to distribution functions
#'
rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[1])

rl_mw[2]


## Quantiles from equivalent estimated GEV models
#'
est_rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                 alpha = alpha,
                                                 confidence_level = 0.95,
                                                 do.ci = TRUE,
                                                 estimator_type = estimator_types[9])

est_rl_pw


## Comparison of estimated quantiles
#'
est_rl_pw_range <- range(as.matrix(est_rl_pw))

est_rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                 alpha = alpha,
                                                 confidence_level = 0.95,
                                                 do.ci = TRUE,
                                                 estimator_type = estimator_types[8])

est_rl_mw_range <- range(as.matrix(est_rl_mw))

#+ fig.width=12, fig.height=8
matplot(x = rownames(est_rl_pw), 
        y = est_rl_pw, 
        xlab = "block size",
        ylab = "quantile",
        main = "Estimates of a quantile",
        ylim = range(c(est_rl_pw_range)),
        cex = 1,
        cex.lab = 1,
        cex.axis = 1,
        type = "l", 
        lty = c("dotted", "solid", "dotted"), 
        lwd = c(2,2,2), 
        col = c(3, 1, 3))

abline(h = rl_mw[2], col = 7, lwd = 2)
abline(h = rl_pw[2], col = 6, lwd = 2)
abline(h = est_rl_pw_range, col = 6, lty = "dotted", lwd = 2)
abline(h = est_rl_mw_range, col = 7, lty = "dotted", lwd = 2)

# Legend:
# yellow: Quantile from GEV mixture model with respect to distribution functions
# pink: Quantile from GEV mixture model with respect to parameters
