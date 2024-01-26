setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

source("./eva_pipeline/src/load_functions.R")

source("./eva_pipeline/src/make_models.R")

source("./eva_pipeline/src/transform_data.R")

source("./eva_pipeline/src/calculate_model_aic.R")

source("./eva_pipeline/src/save_model_aic.R")

source("./eva_pipeline/src/extract_aic.R")

source("./eva_pipeline/src/save_several_model_aic.R")


# use created function
setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

defaultW <- getOption("warn") 
options(warn = -1) 


main_dir  <- "./04_experimentation_01"

response_var <- "lateral_error"

save_several_model_aic(main_dir = main_dir,
                       response_var = response_var,
                       response_abs = FALSE,
                       variable_vetor = c(~name_car, ~name_street.light), 
                       scale_predictors = TRUE,
                       coefficient_iqr = 9, 
                       remove_outliers = FALSE,
                       method = c("interpolate", "mode", "median", "mean")[1])


options(warn = defaultW)



extract_aic(main_dir)






# name_car     name_street.light     name_traffic.sign 
# 138255                110629                103556 
# name_tree         name_tenement     name_tree..group. 
# 77977                 53488                 45694 
# name_traffic.light    name_electric.pole            name_fence 
# 44228                 38739                 34236 
# name_person            name_truck      name_car..group. 
# 32792                 24931                 16318 
# name_house..group.            name_house              name_bus 
# 8985                  8814                  8766 
# name_special.building          name_bicycle         name_airplane 
# 7018                  3615                  1761 
# name_bench        name_stop.sign            name_train 
# 1521                  1316                  1011 
# name_fire.hydrant       name_motorcycle   name_person..group. 
# 644                   529                   344 
# name_bird    name_bench..group.      name_gas.station 
# 320                   238                   213 
# name_helicopter             name_boat              name_dog 
# 133                   132                   109 
# name_scooter           name_tunnel    name_parking.meter 
# 60                    52                    36 
# name_tunnel.entrance     name_bird..group. 
# 23                     2 









