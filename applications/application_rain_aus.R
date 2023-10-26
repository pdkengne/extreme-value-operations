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

