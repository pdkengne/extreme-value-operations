library(readr)

Gnss_imar <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/transfer_5854392_files_fdd9c292/20230911_133812_Record_data_INRIA/Gnss_imar.csv")
Gnss_imar <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/20230911_133812_Record_data_INRIA/Gnss_imar.csv")
#View(Gnss_imar)

Gnss_map_matching <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/transfer_5854392_files_fdd9c292/20230911_133812_Record_data_INRIA/Gnss_map_matching.csv")
Gnss_map_matching <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/20230911_133812_Record_data_INRIA/Gnss_map_matching.csv")
#View(Gnss_map_matching)

Gnss_standard <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/transfer_5854392_files_fdd9c292/20230911_133812_Record_data_INRIA/Gnss_standard.csv")
Gnss_standard <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/20230911_133812_Record_data_INRIA/Gnss_standard.csv")
#View(Gnss_standard)

str(Gnss_imar)

str(Gnss_map_matching)

str(Gnss_standard)


library(tidyverse)

# select relevant data
Gnss_imar_subset <- Gnss_imar %>% select(timestamp, latitude, longitude, altitude)
Gnss_map_matching_subset <- Gnss_map_matching %>% select(timestamp, latitude, longitude, altitude)

# extract timestamp
Gnss_map_matching_subset_timestamp <- Gnss_map_matching_subset$timestamp
Gnss_imar_subset_timestamp <- Gnss_imar_subset$timestamp

# for each timestamp from Gnss_map_matching_subset, find position of the closest timestamp from Gnss_imar_subset
timestamp_position <- sapply(Gnss_map_matching_subset_timestamp, function(ts) which.min(abs(ts - Gnss_imar_subset_timestamp)))

# get errors wrt latitude
latitude_Gnss_map_matching <- Gnss_map_matching$latitude
latitude_Gnss_imar <- Gnss_imar$latitude
error_latitude_Gnss_imar_Gnss_map_matching <- latitude_Gnss_imar[timestamp_position] - latitude_Gnss_map_matching

# get errors wrt longitude
longitude_Gnss_map_matching <- Gnss_map_matching$longitude
longitude_Gnss_imar <- Gnss_imar$longitude
error_longitude_Gnss_imar_Gnss_map_matching <- longitude_Gnss_imar[timestamp_position] - longitude_Gnss_map_matching

# get errors wrt altitude
altitude_Gnss_map_matching <- Gnss_map_matching$altitude
altitude_Gnss_imar <- Gnss_imar$altitude
error_altitude_Gnss_imar_Gnss_map_matching <- altitude_Gnss_imar[timestamp_position] - altitude_Gnss_map_matching


#-------------------------------------------------------------------------------

# select relevant data
Gnss_imar_subset <- Gnss_imar %>% select(timestamp, latitude, longitude, altitude)
Gnss_standard_subset <- Gnss_standard %>% select(timestamp, latitude, longitude, altitude)

# extract timestamp
Gnss_standard_subset_timestamp <- Gnss_standard_subset$timestamp
Gnss_imar_subset_timestamp <- Gnss_imar_subset$timestamp

# for each timestamp from Gnss_standard_subset, find position of the closest timestamp from Gnss_imar_subset
timestamp_position <- sapply(Gnss_standard_subset_timestamp, function(ts) which.min(abs(ts - Gnss_imar_subset_timestamp)))

# get errors wrt latitude
latitude_Gnss_standard <- Gnss_standard$latitude
latitude_Gnss_imar <- Gnss_imar$latitude
error_latitude_Gnss_imar_Gnss_standard <- latitude_Gnss_imar[timestamp_position] - latitude_Gnss_standard

# get errors wrt longitude
longitude_Gnss_standard <- Gnss_standard$longitude
longitude_Gnss_imar <- Gnss_imar$longitude
error_longitude_Gnss_imar_Gnss_standard <- longitude_Gnss_imar[timestamp_position] - longitude_Gnss_standard

# get errors wrt altitude
altitude_Gnss_standard <- Gnss_standard$altitude
altitude_Gnss_imar <- Gnss_imar$altitude
error_altitude_Gnss_imar_Gnss_standard <- altitude_Gnss_imar[timestamp_position] - altitude_Gnss_standard


source("./src/generate_gev_sample.R")
source("./src/calculate_gev_inverse_cdf.R")
source("./src/find_minimum_block_size.R")
source("./src/estimate_gev_mixture_model_parameters.R")
source("./src/plot_gev_mixture_model_pdf.R")
source("./src/calculate_gev_mixture_model_quantile.R")
source("./src/plot_several_standardized_block_maxima_mean.R")
source("./src/estimate_gev_mixture_model_quantile.R")

# extreme value analysis of errors wrt latitude

coefficient <- 10^(0)

x <- coefficient*error_altitude_Gnss_imar_Gnss_map_matching

n <- length(x)
n

nlargest <- 1000

hist(x)

range(x)

threshold <- quantile(x = x, probs = 0.5)
threshold

hist(x[x>threshold])

gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
                                                           nsloc = NULL,
                                                           std.err = FALSE,
                                                           block_sizes = NULL,
                                                           minimum_nblocks = 50,
                                                           threshold = threshold,
                                                           nlargest = nlargest,
                                                           confidence_level = 0.95,
                                                           trace = TRUE)

gev_mixture_model$normalized_gev_parameters_object

gev_mixture_model$weighted_normalized_gev_parameters_object

gev_mixture_model$automatic_weights_mw_statistics

gev_mixture_model$automatic_weights_pw_statistics

gev_mixture_model$pessimistic_weights_pw_shape

gev_mixture_model$automatic_weights_mw


estimator_types <- c("automatic_weights_mw",
                    "pessimistic_weights_mw",
                    "identic_weights_mw",
                    "automatic_weights_pw",
                    "pessimistic_weights_pw",
                    "identic_weights_pw",
                    "empirical",
                    "confidence_interval_mw",
                    "confidence_interval_pw")


alpha <- 10^(-14)

results_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                               alpha = alpha,
                                               confidence_level = 0.95,
                                               do.ci = TRUE,
                                               estimator_type = estimator_types[1])

results_mw

results_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                  alpha = alpha,
                                                  confidence_level = 0.95,
                                                  do.ci = TRUE,
                                                  estimator_type = estimator_types[4])

results_pw

quantile(x = x, probs = 1 - alpha)

est_rl_pw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                  alpha = alpha,
                                                  confidence_level = 0.95,
                                                  do.ci = TRUE,
                                                  estimator_type = estimator_types[9])

est_rl_pw

est_rl_pw_range <- range(as.matrix(est_rl_pw))
est_rl_pw_range


est_rl_mw <- estimate_gev_mixture_model_quantile(gev_mixture_model,
                                                 alpha = alpha,
                                                 confidence_level = 0.95,
                                                 do.ci = TRUE,
                                                 estimator_type = estimator_types[8])

est_rl_mw

est_rl_mw_range <- range(as.matrix(est_rl_mw))
est_rl_mw_range


matplot(rownames(est_rl_pw), est_rl_pw, type = "l", lty = c("dotted", "solid", "dotted"), lwd = 2, col = c(3, 1, 3))

abline(h = results_mw[2], col = 7, lwd = 2)
abline(h = results_pw[2], col = 6, lwd = 2)
abline(h = est_rl_pw_range, col = 6, lty = "dotted", lwd = 2)
abline(h = est_rl_mw_range, col = 7, lty = "dotted", lwd = 2)


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

