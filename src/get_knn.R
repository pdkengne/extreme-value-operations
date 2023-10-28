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







