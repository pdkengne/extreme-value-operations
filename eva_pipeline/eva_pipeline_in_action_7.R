setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")


source("./eva_pipeline/src/load_functions.R")

source("./eva_pipeline/src/read_data.R")

source("./eva_pipeline/src/make_models.R")

source("./eva_pipeline/src/transform_data.R")

source("./eva_pipeline/src/calculate_model_aic.R")

source("./eva_pipeline/src/save_multiple_model_aic.R")

source("./eva_pipeline/src/extract_multiple_model_aic.R")



# use created function
defaultW <- getOption("warn") 
options(warn = -1) 


main_dir  <- "./04_experimentation_03"

response_var_vector <- c("lateral_error", "longitudinal_error")


variable_vector <- c(~1, ~velocity, ~area, ~object, ~name_car, ~name_street.light, 
                     ~name_traffic.sign, ~name_tree, ~name_tenement, ~name_tree..group.,
                     ~name_traffic.light, ~name_electric.pole, ~name_fence, ~name_person,
                     ~name_truck, ~name_car..group., ~name_house..group., ~name_house, 
                     ~name_bus, ~name_special.building, ~horizontal_left, ~horizontal_right, 
                     ~vertical_up, ~vertical_down)


skip <- 14394
# skip <- 0

iqr <- 3


save_multiple_model_aic(main_dir = main_dir,
                        response_var_vector = response_var_vector[1],
                        response_abs = TRUE,
                        nrow_skip = skip,
                        variable_vector = variable_vector, 
                        scale_predictors = TRUE,
                        coefficient_iqr = iqr, 
                        iterate = 10,
                        remove_outliers = TRUE,
                        method = c("interpolate", "mode", "median", "mean")[1])



options(warn = defaultW)



extract_multiple_model_aic(main_dir, response_var_vector)



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









