setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

library(plotly)

source <- "./applications/final_dataset.csv"
data <-  read.csv(file = source, sep = ",")



fig <- plot_ly(data, x = ~longitude, y = ~latitude, z = ~lateral_error)

fig <- fig %>% add_markers(size = 3)

fig <- fig %>% layout(scene = list(xaxis = list(title = 'longitude'),
                                   
                                   yaxis = list(title = 'latitude'),
                                   
                                   zaxis = list(title = 'lateral_error')))

fig



fig <- plot_ly(data, x = ~longitude, y = ~latitude, z = ~longitudinal_error)

fig <- fig %>% add_markers(size = 3)

fig <- fig %>% layout(scene = list(xaxis = list(title = 'longitude'),
                                   
                                   yaxis = list(title = 'latitude'),
                                   
                                   zaxis = list(title = 'longitudinal_error')))

fig


fig <- plot_ly(data, x = ~longitude, y = ~latitude, z = ~haversine_error)

fig <- fig %>% add_markers(size = 3)

fig <- fig %>% layout(scene = list(xaxis = list(title = 'longitude'),
                                   
                                   yaxis = list(title = 'latitude'),
                                   
                                   zaxis = list(title = 'haversine_error')))

fig


fig <- plot_ly(na.omit(data), x = ~velocity_longitude, y = ~velocity_latitude, z = ~lateral_error)

fig <- fig %>% add_markers(size = 3)

fig <- fig %>% layout(scene = list(xaxis = list(title = 'velocity_longitude'),
                                   
                                   yaxis = list(title = 'velocity_latitude'),
                                   
                                   zaxis = list(title = 'lateral_error')))

fig


fig <- plot_ly(na.omit(data), x = ~velocity_longitude, y = ~velocity_latitude, z = ~longitudinal_error)

fig <- fig %>% add_markers(size = 3)

fig <- fig %>% layout(scene = list(xaxis = list(title = 'velocity_longitude'),
                                   
                                   yaxis = list(title = 'velocity_latitude'),
                                   
                                   zaxis = list(title = 'longitudinal_error')))

fig


fig <- plot_ly(na.omit(data), x = ~velocity_longitude, y = ~velocity_latitude, z = ~haversine_error)

fig <- fig %>% add_markers(size = 3)

fig <- fig %>% layout(scene = list(xaxis = list(title = 'velocity_longitude'),
                                   
                                   yaxis = list(title = 'velocity_latitude'),
                                   
                                   zaxis = list(title = 'haversine_error')))

fig
