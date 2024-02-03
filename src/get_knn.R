# library(dbscan)
# library(FNN)

get_knn <- function(data, k, query = NULL, search = "kdtree"){
  # data: a data matrix
  # k: the maximum number of nearest neighbors to search
  # query: a data matrix with the points to query
  # search: nearest neighbor search strategy (one of "kdtree", "linear" or "dist")
  
  output <- dbscan::kNN(x = data,
                        k = k, 
                        query = query, 
                        search = search,
                        sort = TRUE)
  
  output
}


# example 1

data <- data.frame(cbind(1:10, 1:10))

query <- data[1, ]

k <- 3

result <- get_knn(data = data, k = k, query = query)
result


# get the nearest neighbor indices
result$id

# get the nearest neighbor Euclidean distances
result$dist


# example 2

data <- data.frame(cbind(1:10, 1:10))

query <- NULL

k <- 3

result <- get_knn(data = data, k = k, query = query)
result

# get the nearest neighbor indices
result$id

# get the nearest neighbor Euclidean distances
result$dist
