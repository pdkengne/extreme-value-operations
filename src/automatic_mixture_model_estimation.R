# Exécuter DBSCAN pour la détection de la multimodalité

# library(dbscan)
# library(bmixture)
# library(factoextra)
# library(zoo)
# library(FactoMineR)


x <- bmixture::rmixnorm(n = 1500, weight = c(0.5, 0.5, 0.5), mean = c(-3, 3, 9), sd = c(1, 1, 1)) 

dens_x <- density(x)
hist(x, prob = TRUE, ylim = range(dens_x$y))
lines(dens_x, lwd = 2, col = 4)

z <- cumsum(dens_x$y)/sum(dens_x$y)

plot(dens_x$x,z)  

data <- data.frame(cbind(dens_x$x, dens_x$y), z)

head(data)

max_vec <- zoo::rollmax(x = dens_x$y, k = 20, align = c("center", "left", "right"))

plot(max_vec)

sort(table(max_vec))

modes <- which(table(max_vec) >= 20)

modes

k <- length(modes)


set.seed(123)
km.res <- kmeans(data, k, nstart = 25)
fviz_cluster(km.res, data[ ,c(1, 2)], ellipse = TRUE, geom = "point")
fviz_cluster(km.res, data[ ,c(1, 3)], ellipse = TRUE, geom = "point")



min_vec <- -zoo::rollmax(x = -dens_x$y, k = 20, align = c("center", "left", "right"))

plot(min_vec)

sort(table(min_vec))

bounds <- which(table(min_vec) >= 20)

bounds

b <- as.numeric(names(bounds))
b

range(x)


# Effectuer une analyse factorielle
#resultats_factorielle <- PCA(data)


# Effectuer une analyse HCPC sur les résultats de l'analyse factorielle
#resultats_HCPC <- HCPC(resultats_factorielle)


# plot(data[,1:2],col=rainbow(2)[factor(resultats_HCPC$data.clust$clust)],pch=16) 
#fviz_cluster(resultats_HCP, data[, c(1,2)], ellipse.type = "norm")


# dbscan_result <- dbscan::dbscan(data, eps = 5, minPts = 20)

dbscan_result

names(dbscan_result)

plot(data[ ,c(1, 2)], col=dbscan_result$cluster+1)

plot(data[ ,c(1, 3)], col=dbscan_result$cluster+1)


plot(data[ ,c(1, 2)], col=km.res$cluster+1)

plot(data[ ,c(1, 3)], col=km.res$cluster+1)


data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, frame = FALSE, geom = "point")

