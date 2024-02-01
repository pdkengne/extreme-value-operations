setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations/eva_pipeline")

library(targets)
library(tarchetypes)

tar_manifest(fields = all_of("command"))

tar_visnetwork()

tar_make()

tar_watch()

tar_read()

tar_load()

tar_make()

tar_visnetwork()

tar_outdated()

tar_watch_ui()

# Génération rapport
tar_render(report_object, "reports/evolution.Rmd")

# supprime la totalite du répertoire _targets
tar_destroy() 

# supprime l’objet donnees du cache
tar_delete(donnees) 

# permet de supprimer les cibles qui ne sont plus présentes dans le pipeline
tar_prune() 


tar_load(aic_path)

aic_path









