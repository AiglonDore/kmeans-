# Step 1
kmeansplusplus_init <- function(data, k) {
    # k is the number of clusters
    dim <- nrow(data)
    output = list()
    x <- runif(n = 1, min = 1, max = dim)
    output <- append(output, list(data[x,]))
    distance.matrix <- as.matrix(dist(data))
    for (i in 2:k) { # k clusters
        distList <- list()
        for (j in 1:dim) { # Distances from point to closest center
            dists = list()
            for (k in 1:length(output)) { #Distances from point to centers
                dist <- distance.matrix[j,k]
                dists <- append(dists, dist)
            } 
            distList <- append(distList, min(unlist(dists)))
        }
        
        x <- sample(x = 1:dim, size = 1, prob = unlist(distList) / sum(unlist(distList))) #If an element is already chose, then its distance will be 0 so it can't be chosen again.
        output <- append(output, list(data[x, ]))
    }
    return(output)
}

# Complete algorithm
kmeansplusplus <- function(data, k){
    centers <- kmeansplusplus_init(data, k)
    df <- as.data.frame(matrix(unlist(centers), ncol = ncol(data), byrow = TRUE))
    return(kmeans(data, df))
}