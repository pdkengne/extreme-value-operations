setwd("~/Documents/Doc-perso-2023/Job-Valeo/evops-project/extreme-value-operations")

# time series: https://otexts.com/fpp2/
# https://robjhyndman.com/hyndsight/tsoutliers/
# https://delladata.fr/comment-detecter-les-outliers-avec-r/
# https://rpubs.com/Alema/1000582
# https://essicolo.github.io/ecologie-mathematique-R/chapitre-ode.html

# If Q1Q1 denotes the 25th percentile and Q3Q3 denotes the 75th percentile of the remainder values, 
# then the interquartile range is defined as IQR=Q3−Q1IQR=Q3−Q1. Observations are labelled as outliers
# if they are less than Q1−3×IQRQ1−3×IQR or greater than Q3+3×IQRQ3+3×IQR. 
# This is the definition used by Tukey (1977, p44) in his original boxplot proposal for “far out” values.
# 
# If the remainder values are normally distributed, then the probability of an observation 
# being identified as an outlier is approximately 1 in 427000.
# 
# Any outliers identified in this manner are replaced with linearly interpolated values 
# using the neighbouring observations, and the process is repeated.


# travailler avec les erreurs en valeurs absolues

# identification des outliers à l'aide d'un boxplot
# extraire la séries des outliers avec les covariables correspondantes, puis faire une eda sur ces données
# identifier et supprimer les outliers, puis faire une eva sur ces données
# identifier et remplacer les outliers, puis faire uneeva sur ces données



library(tidyverse)

library(DescTools)

library(dlookr)

library(forecast)


source <- "./applications/final_dataset.csv"
data <-  read.csv(file = source, sep = ",")

names(data)

df <- data %>% select(lateral_error) %>% mutate(lateral_error_abs = abs(lateral_error))

lateral_error_abs <- imputate_outlier(df, lateral_error_abs, method = "mode", cap_ntiles = c(0.05, 0.90))

lateral_error_abs <- tsoutliers(x = df$lateral_error_abs, iterate = 2, lambda = NULL)

names(lateral_error_abs)
# "index"        "replacements"

summary(lateral_error_abs)

plot(lateral_error_abs)

hist(lateral_error_abs)

plot(df$lateral_error_abs, type = "l")
lines(lateral_error_abs, col = 4)
hist(lateral_error_abs)

plot(df$lateral_error_abs, type = "l", cex = 0.1)
abline(h = 0, lty = "dotted", col = 7)
points(lateral_error_abs$index, lateral_error_abs$replacements, col = 2, cex = 0.1)


lateral_error_abs_bis <- df$lateral_error_abs
lateral_error_abs_bis[lateral_error_abs$index] <- lateral_error_abs$replacements

boxplot(lateral_error_abs_bis)
hist(lateral_error_abs_bis)
acf(lateral_error_abs_bis)

plot(df$lateral_error_abs, type = "l")
abline(h = 0, lty = "dotted", col = 7)
lines(lateral_error_abs_bis, col = 4)
hist(lateral_error_abs_bis)

outliers_1 <- Outlier(x = data$lateral_error, method = c("boxplot", "hampel")[1], value = TRUE, na.rm = FALSE)

outliers_2 <- Outlier(x = data$lateral_error, method = c("boxplot", "hampel")[2], value = TRUE, na.rm = FALSE)

plot(outliers_1)
plot(outliers_2)


data_2 <- data %>% select(!c(timestamp, latitude, longitude, velocity_latitude, velocity_longitude,
                             velocity, lateral_error, longitudinal_error, haversine_error, latitude_error,
                             longitude_error, heading_error, area, object))

names(data_2)

tail(data_2)

res <- prcomp(data_2, center = TRUE, scale = TRUE)

summary(res)

factoextra::get_eig(res)

factoextra::fviz_screeplot(res, addlabels = TRUE)

factoextra::fviz_pca_var(res)

factoextra::fviz_contrib(res, choice = "var", axes = 1)





data <- scale(data)

head(data)

resCAH <- factoextra::hcut(data_2,  hc_method = "average", 
                           hc_metric = "euclidean", stand = TRUE)
resCAH

datas=scale(data_2)
resKM <- kmeans(datas, centers=3,nstart=20)
resKM


datas=scale(data)
resKM <- kmeans(datas, centers = 3,nstart=20)
resKM


factoextra::fviz_nbclust(data_2, 
                         FUNcluster = factoextra::hcut, 
                         method = "silhouette",
                         hc_method = "average", 
                         hc_metric = "euclidean", 
                         stand = TRUE)


# Clustering hiérarchique = CAH
# hc_method: “single”, “complete”, “average”, “ward.D”, “ward.D2”
# hc_func: "hclust", "agnes", "diana"

resCAH <- factoextra::hcut(data, k = .., hc_method = ..., 
                           hc_metric = "euclidean", stand = TRUE, graph = TRUE)








res <- hcut(data_2, k = 3, stand = TRUE)

res$cluster

res$size

#fviz_dend(res, rect = TRUE)

fviz_cluster(res)

#-------------------------------------------------------------------------------

library(factoextra)

data(USArrests)

str(USArrests)

# Compute hierarchical clustering and cut into 4 clusters
res <- hcut(USArrests, k = 4, stand = TRUE)

# Cluster assignements of observations
res$cluster
# Size of clusters
res$size

# Visualize the dendrogram
fviz_dend(res, rect = TRUE)

# Visualize the silhouette
fviz_silhouette(res)

# Visualize clusters as scatter plots
fviz_cluster(res)

#-------------------------------------------------------------------------------

#on crée un jeu de donnée 

b1<-c(0.1, 0.2,6,5,5,6,7,8,8,9,9,9,10,10,25)

#on trace le boxplot

boxplot(b1) #il y a 3 outliers 

#on met le boxplot dans un objet box

box<-boxplot(b1, range = 2.5)
boxplot(b1, range = 2.5)

out <- box$out
out
out_ind <- which(b1 %in% c(out))
out_ind

#box$out donne les outliers

#on crée des nouvelles données sans les outliers

b2<-b1[-which(b1%in%box$out)]

#on vérifie

boxplot(b2)


out <- boxplot.stats(dat$hwy)$out
out_ind <- which(dat$hwy %in% c(out))
out_ind
