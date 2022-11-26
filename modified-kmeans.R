source(file = "kmeans.R")

# We are going to only reimplement some steps, not the whole algorithm
# Only the first step will be reimplemented.

# Step 1
kmeansplusplus_init <- function(d, k) {
    # d is the dimension
    # k is the number of clusters
    output <- list()
    # First centroid is chosen randomly
    x <- floor(runif(n = 1, min = 0, max = (d - 1)))
    output <- append(output, x)
    for (i in 2:k) {
        # For each centroid, we compute the distance to the closest centroid
        dist <- list()
        for (j in 1:(length(output) - 1)) {
            y <- output[[j]]
            dist <- append(dist, sqrt(sum((x - y)^2)))
        }
        # We choose the next centroid, proportionally to the distance
        # to the closest centroid
        x <- sample(x = 1:d, size = 1, prob = dist / sum(dist))
        x <- list(x)
        output <- append(output, x)
    }
    return(output)
}

# Complete algorithm

kmeansplusplus <- function(data, k, max_iter = NULL) {
    # data is the data set
    # k is the number of clusters
    # max_iter is the maximum number of iterations, if NULL, then no limit
    d <- nrow(data)
    index <- kmeansplusplus_init(d, k)
    centroids <- list()
    for (i in 1:(length(index) - 1)) {
        centroids <- append(centroids, data[index[[i]], ])
    }
    centroids2 <- list()
    i <- 0
    while (centroids != centroids2 || (max_iter != NULL && i < max_iter)) {
        i <- i + 1
        assignments <- kmeans_assign(data, centroids)
        centroids2 <- centroids
        centroids <- kmeans_update(data, centroids, assignments)
    }
    return(centroids)
}
