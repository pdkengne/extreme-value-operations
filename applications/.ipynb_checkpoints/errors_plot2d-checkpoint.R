setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

library(plotly)

source <- "./applications/final_dataset.csv"
data <-  read.csv(file = source, sep = ",")

data$index <- 1:nrow(data)


fig <- plot_ly(data, x = ~index, y = ~lateral_error, name = 'lateral_error', type = 'scatter', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~longitudinal_error, name = 'longitudinal_error', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~haversine_error, name = 'haversine_error', mode = 'lines+markers')

fig


fig <- plot_ly(data, x = ~index, y = ~data$velocity, name = 'velocity', type = 'scatter', mode = 'lines+markers')
fig


fig <- plot_ly(data, x = ~index, y = ~data$velocity_latitude, name = 'velocity_latitude', type = 'scatter', mode = 'lines+markers')
fig


fig <- plot_ly(data, x = ~index, y = ~data$velocity_longitude, name = 'velocity_longitude', type = 'scatter', mode = 'lines+markers')
fig



fig <- plot_ly(data, x = ~index, y = ~data$latitude_error, name = 'latitude_error', type = 'scatter', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~data$longitude_error, name = 'longitude_error', mode = 'lines+markers')
fig
