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


altitude_Gnss_imar <- Gnss_imar$altitude

altitude_Gnss_standard <- Gnss_standard$altitude

error_altitude_Gnss_imar_Gnss_standard <- altitude_Gnss_imar - altitude_Gnss_standard

length(error_altitude_Gnss_imar_Gnss_standard)

boxplot(error_altitude_Gnss_imar_Gnss_standard)

hist(error_altitude_Gnss_imar_Gnss_standard)

range(error_altitude_Gnss_imar_Gnss_standard)



# example 1

source("./src/generate_gev_sample.R")
source("./src/calculate_gev_inverse_cdf.R")

source("./src/estimate_gev_mixture_model_parameters.R")
source("./src/plot_gev_mixture_model_pdf.R")
source("./src/calculate_gev_mixture_model_quantile.R")
source("./src/plot_several_standardized_block_maxima_mean.R")


x <- error_altitude_Gnss_imar_Gnss_standard[error_altitude_Gnss_imar_Gnss_standard > 0.5*max(error_altitude_Gnss_imar_Gnss_standard)]

hist(x)

n <- length(x)
n

nlargest <- 1000

y <- extract_nlargest_sample(x, n = nlargest)

hist(y)


blocks <- get_candidate_block_sizes(x, m = 50)
blocks

model <- estimate_single_gev_model(x, block_size = 300, nsloc = NULL)

model$normalized_gev_parameters

min_block <- find_minimum_block_size(y)
min_block

maxima <- extract_block_maxima(y, block_size = 100)
maxima


blocks <- 1:20

plot_several_standardized_block_maxima_mean(x = y, blocks, confidence_level = 0.95, equivalent = FALSE)
plot_several_standardized_block_maxima_mean(x = y, blocks, confidence_level = 0.95, equivalent = TRUE)

models <- estimate_several_gev_models(x, block_sizes = blocks, nsloc = NULL)

names(models)

models$normalized_gev_parameters_object


gev_mixture_model <- estimate_gev_mixture_model_parameters(x,
                                                           nsloc = NULL,
                                                           std.err = FALSE,
                                                           block_sizes = 1:20,
                                                           minimum_nblocks = 50,
                                                           nlargest = nlargest,
                                                           confidence_level = 0.95,
                                                           trace = TRUE)

gev_mixture_model$normalized_gev_parameters_object

gev_mixture_model$weighted_normalized_gev_parameters_object

gev_mixture_model$automatic_weights_mw_statistics

gev_mixture_model$automatic_weights_pw_statistics

gev_mixture_model$automatic_weights_pw_shape



alpha <- 0.01

results <- calculate_gev_mixture_model_quantile(gev_mixture_model,
                                                alpha = alpha,
                                                confidence_level = 0.95)

results

quantile(x = x, probs = 1 - alpha)


est_rl <- results$estimated_gev_model_quantile_unrestricted_weight


matplot(rownames(est_rl), est_rl, type = "l", lty = "dotted")


abline(h = results$estimated_automatic_weighted_gev_mixture_model_quantile_pw, col = 5)
abline(h = results$estimated_automatic_weighted_gev_mixture_model_quantile_mw, col = 6)





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
# 
# 
