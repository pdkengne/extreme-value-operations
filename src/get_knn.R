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

nn <- get_knn(data = data, k = k, query = query)
nn

# get the nearest neighbor indices
nn$id

# get the nearest neighbor Euclidean distances
nn$dist


# example 2

data <- data.frame(cbind(1:10, 1:10))

query <- NULL

k <- 3

nn <- get_knn(data = data, k = k, query = query)
nn

# get the nearest neighbor indices
nn$id

# get the nearest neighbor Euclidean distances
nn$dist
