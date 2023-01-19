source("kmeans.R")
source("modified-kmeans.R")
source("weight.R")

generate_sets <- function(n, nbcenters, dim = 5){
    centers = c()
    for (i in 1:nbcenters){
        centers <- append(centers, c(runif(n = dim, min = 0, max = 500)))
    }
    output <- c()
    for (x in centers){
        for (i in 1:n){
            noise = rnorm(n = dim, mean = x, sd = 1)
            output <- append(output, c(noise + x))
        }
    }
    return(output)
}

generate_norms <- function(n, nbcenters, dim = 5){
    x <- generate_sets(n, nbcenters ,dim)
    m <- data.frame(matrix(x, ncol = dim))
    return(m)
}

#We use 1000 instead of 10000 because 10000 points took too much time to compile.
NORM10 <-generate_norms(100, 10, 5)
NORM25 <-generate_norms(100, 25, 5)

#Kmeans

for (k in c(10,25,50)){
    weight_kmeans10 <- c()
    weight_kmeans25 <- c()
    weight_kmeans10pp <- c()
    weight_kmeans25pp <- c()
    for (i in 1:20){
        means10 <- kmeans(NORM10, k)
        means25 <- kmeans(NORM25, k)
        means10pp <- kmeansplusplus(NORM10, k)
        means25pp <- kmeansplusplus(NORM25, k)
        
        centroids10 <- matrix(means10, ncol = 5)
        centroids25 <- matrix(means25, ncol = 5)
        centroids10pp <- matrix(means10, ncol = 5)
        centroids25pp <- matrix(means25, ncol = 5)
        
        weight_kmeans10 <- append(weight_kmeans10, phi(NORM10, centroids10))
        weight_kmeans25 <- append(weight_kmeans25, phi(NORM25, centroids25))
        weight_kmeans10pp <- append(weight_kmeans10pp, phi(NORM10, centroids10))
        weight_kmeans25pp <- append(weight_kmeans25pp, phi(NORM25, centroids25))
    }
    print(paste("k = ", k))
    print("K-means")
    print(paste("Average cost (10): ", mean(weight_kmeans10)))
    print(paste("Average cost (25): ", mean(weight_kmeans25)))
    print(paste("Minimum cost (10): ", min(weight_kmeans10)))
    print(paste("Minimum cost (25): ", min(weight_kmeans25)))
    print("K-means++")
    print(paste("Average cost (10): ", mean(weight_kmeans10pp)))
    print(paste("Average cost (25): ", mean(weight_kmeans25pp)))
    print(paste("Minimum cost (10): ", min(weight_kmeans10pp)))
    print(paste("Minimum cost (25): ", min(weight_kmeans25pp)))
}