# library(FNN)
# library(dbscan)


data <- cbind(1:10, 1:10)

data

query <- data

FNN::get.knn(data, k=5)

FNN::get.knnx(data, query[1:2,], k=5)


data(iris)
x <- iris[, -5]

# Example 1: finding kNN for all points in a data matrix (using a kd-tree)
nn <- dbscan::kNN(x = data, query = query[1,], k = 5)
nn

nn <- dbscan::kNN(x = data, k = 5)
nn
# explore neighborhood of point 10
i <- 10
nn$id[i,]
plot(x, col = ifelse(1:nrow(iris) %in% nn$id[i,], "red", "black"))

# visualize the 5 nearest neighbors
plot(nn, x)

# visualize a reduced 2-NN graph
plot(dbscan::kNN(nn, k = 2), x)

# Example 2: find kNN for query points
q <- x[c(1,100),]
nn <- dbscan::kNN(x, k = 10, query = q)

plot(nn, x, col = "grey")
points(q, pch = 3, lwd = 2)


# Charger les données Iris

data(iris)


# Exécuter DBSCAN sur les 4 premières colonnes du jeu de données

#library(dbscan)

dbscan_result <- dbscan::dbscan(iris[, 1:4], eps = 10, minPts = 5)


# Afficher les résultats

dbscan_result

names(dbscan_result)

plot(iris[ ,1:2],col=dbscan_result$cluster+1)


predict(object = dbscan_result, newdata = iris[c(1,99,150), 1:4], data = iris[,1:4])


# Exécuter DBSCAN pour la détection de la multimodalité

# library(dbscan)
# library(bmixture)

x <- bmixture::rmixnorm(n = 1000, weight = c(0.5, 0.5), mean = c(-3, 3), sd = c(1, 1)) 

dens_x <- density(x)
hist(x, prob = TRUE, ylim = range(dens_x$y))
lines(dens_x, lwd = 2, col = 4)

z <- cumsum(dens_x$y) 

plot(dens_x$x,z)  

data <- data.frame(cbind(dens_x$x, dens_x$y), z)

head(data)


dbscan_result <- dbscan::dbscan(data, eps = 5, minPts = 20)


dbscan_result

names(dbscan_result)

plot(data[ ,c(1, 2)], col=dbscan_result$cluster+1)

plot(data[ ,c(1, 3)], col=dbscan_result$cluster+1)




