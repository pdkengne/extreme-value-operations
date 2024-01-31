setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations/etl_pipeline")

library(targets)
library(tarchetypes)

tar_manifest(fields = all_of("command"))

tar_visnetwork()

tar_make()

tar_read(plot)

tar_make()

tar_visnetwork()

tar_outdated()

tar_watch_ui()



























