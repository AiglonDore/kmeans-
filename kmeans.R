# Step 1
kmeans_init <- function(d, k) {
    # d is the dimension
    # k is the number of clusters
    output <- list(floor(runif(n = d, min = 0, max = 100)))
    for (i in 2:k) {
        x <- floor(runif(n = d, min = 0, max = 100))
        x <- list(x)
        output <- append(output, x)
    }
    return(output)
}

# Step 2
kmeans_assign <- function(data, centroids) {
    # data is the data set
    # centroids is the list of centroids
    output <- list()
    for (i in 1:(length(centroids) - 1)) {
        x <- data[i, ]
        dist <- list()
        for (j in 1:(length(centroids) - 1)) {
            y <- centroids[[j]]
            dist <- append(dist, sqrt(sum((x - y)^2)))
        }
        output <- append(output, which.min(dist))
    }
    return(output)
}

# Step 3
kmeans_update <- function(data, centroids, assignments) {
    # data is the data set
    # centroids is the list of centroids
    # assignments is the list of assignments
    output <- list()
    for (i in 1:(length(centroids) - 1)) {
        x <- data[assignments == i, ]
        output <- append(output, colMeans(x))
    }
    return(output)
}

# Complete algorithm

kmeans <- function(data, k, max_iter = NULL) {
    # data is the data set
    # k is the number of clusters
    # max_iter is the maximum number of iterations, if NULL, then no limit
    d <- ncol(data)
    centroids <- kmeans_init(d, k)
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
