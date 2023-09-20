library(readr)

Gnss_imar <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/transfer_5854392_files_fdd9c292/20230911_133812_Record_data_INRIA/Gnss_imar.csv")
View(Gnss_imar)

Gnss_map_matching <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/transfer_5854392_files_fdd9c292/20230911_133812_Record_data_INRIA/Gnss_map_matching.csv")
View(Gnss_map_matching)

Gnss_standard <- read_csv("~/Documents/Doc-perso-2023/Job-Valeo/transfer_5854392_files_fdd9c292/20230911_133812_Record_data_INRIA/Gnss_standard.csv")
View(Gnss_standard)

str(Gnss_imar)

str(Gnss_map_matching)

str(Gnss_standard)
