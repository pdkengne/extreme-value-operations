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
#str(weatherAUS)

#'
names(weatherAUS)

#' 
head(weatherAUS)

#'
tail(weatherAUS)

# plot_str(weatherAUS)

# introduce(weatherAUS)

#+ fig.width=12, fig.height=14
plot_intro(weatherAUS)

#+ fig.width=12, fig.height=15
plot_missing(weatherAUS)

#'
final_data <- drop_columns(weatherAUS, c("Date", "RISK_MM", "Cloud9am", "Cloud3pm", "Evaporation", "Sunshine", "WindGustDir",
                                         "WindGustSpeed", "WindDir9am", "Pressure9am", "Pressure3pm", "RainToday", "RainTomorrow"))

# names(final_data)

# View(final_data)

# profile_missing(final_data)


#+ fig.width=12, fig.height=12
plot_bar(final_data)

#+ fig.width=12, fig.height=14
plot_histogram(final_data)

#+ fig.width=12, fig.height=12
plot_correlation(na.omit(final_data), type = "c")

#+ fig.width=12, fig.height=14
plot_scatterplot(final_data[, c("MinTemp", "MaxTemp", "Rainfall",  "WindSpeed9am",  "WindSpeed3pm",  "Humidity9am",  
                                "Humidity3pm",  "Temp9am", "Temp3pm")], 
                 by = "Rainfall",
                 sampled_rows = 3000L)


#'
final_data_clean <- na.omit(final_data)

summary(final_data_clean)

# View(final_data_clean)


#'
x <- final_data_clean$Rainfall
x <- x[!is.na(x)]
n <- length(x)

n

# Histogram of all data
#+ fig.width=12, fig.height=8
dens_x <- density(x)
hist(x, prob = TRUE, ylim = range(dens_x$y))
lines(dens_x, lwd = 2, col = 4)

# Autocorrelation function of all data
#+ fig.width=12, fig.height=8
acf(x)


# Histogram of the largest data
#'
nlargest <- 2000
y <- extract_nlargest_sample(x, n = nlargest)
dens_y <- density(y)
hist(y, prob = TRUE, ylim = range(dens_y$y))
lines(density(y), lwd = 2, col = 4)

# Autocorrelation function of the largest data
#+ fig.width=12, fig.height=8
acf(y)

# Estimation of gev mixture models
#'
gev_mixture_model <- suppressWarnings(estimate_gev_mixture_model_parameters(x = x, 
                                                                            block_sizes = 10:40,
                                                                            minimum_nblocks = 50,
                                                                            threshold = NULL,
                                                                            nlargest = nlargest,
                                                                            confidence_level = 0.95,
                                                                            use_extremal_index = TRUE,
                                                                            use_lower_threshold = FALSE,
                                                                            maximum_iterations = 1500,
                                                                            log_mv = TRUE,
                                                                            log_pw = TRUE,
                                                                            trace = FALSE,
                                                                            method = "MLE"))

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
#+ fig.width=12, fig.height=8
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = FALSE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

## GEV mixture model with respect to distribution functions
#+ fig.width=12, fig.height=8
plot_gev_mixture_model_pdf(gev_mixture_model,
                           type = "automatic_weights",
                           model_wise = TRUE,
                           zoom = TRUE,
                           xlab = "Quantile",
                           ylab = "Density",
                           main = "Probability Density Function (PDF) Plot")

# Estimation of an extreme quantile
#'
estimator_types <- c("automatic_weights_mw", 
                     "pessimistic_weights_mw", 
                     "identic_weights_mw", 
                     "automatic_weights_pw",
                     "pessimistic_weights_pw", 
                     "identic_weights_pw", 
                     "model_wise",
                     "parameter_wise",
                     "empirical")

#'
alpha <- 10^(-6)

## Quantile from GEV mixture model with respect to parameters
#'
rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[4])

rl_pw


## Quantile from GEV mixture model with respect to distribution functions
#'
rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                             alpha = alpha,
                                             confidence_level = 0.95,
                                             do.ci = TRUE,
                                             estimator_type = estimator_types[1])

rl_mw


## Quantiles from equivalent estimated distributions in GEV mixture model with respect to parameters
#'
est_rl_pw <- suppressWarnings(estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                                  alpha = alpha,
                                                                  confidence_level = 0.95,
                                                                  do.ci = TRUE,
                                                                  estimator_type = estimator_types[8]))

est_rl_pw


## Comparison of estimated quantiles
#'
est_rl_pw_range <- range(as.matrix(est_rl_pw))


## Quantiles from equivalent estimated GEV distributions in GEV mixture model respect to distribution functions
#'
est_rl_mw <- suppressWarnings(estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                                  alpha = alpha,
                                                                  confidence_level = 0.95,
                                                                  do.ci = TRUE,
                                                                  estimator_type = estimator_types[7]))
est_rl_mw

est_rl_mw_range <- range(as.matrix(est_rl_mw))

est_rl_mw_range

#+ fig.width=12, fig.height=8
matplot(x = rownames(est_rl_pw), 
        y = est_rl_pw, 
        xlab = "block size",
        ylab = "quantile",
        main = "Estimates of a quantile",
        ylim = range(c(est_rl_pw_range, rl_pw)),
        cex = 1,
        cex.lab = 1,
        cex.axis = 1,
        type = "l", 
        lty = c("dotted", "solid", "dotted"), 
        lwd = c(2,2,2), 
        col = c(3, 1, 3))

abline(h = rl_mw, col = 7, lwd = 2)
abline(h = rl_pw, col = 6, lwd = 2)
abline(h = est_rl_pw_range, col = 6, lty = "dotted", lwd = 2)
abline(h = est_rl_mw_range, col = 7, lty = "dotted", lwd = 2)

# Legend:
# yellow: Quantile from GEV mixture model with respect to distribution functions
# pink: Quantile from GEV mixture model with respect to parameters

