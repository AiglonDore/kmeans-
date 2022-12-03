source("kmeans.R")
source("modified-kmeans.R")
source("weight.R")

generate_sets <- function(n, nbcenters, dim = 5){
    centers = list()
    for (i in 1:nbcenters){
        centers <- append(centers, list(runif(n = dim, min = 0, max = 500)))
    }
    output <- list()
    for (x in centers){
        for (i in 1:n){
            noise = rnorm(n = dim, mean = x, sd = 1)
            output <- append(output, list(noise + x))
        }
    }
    return(output)
}

generate_norms <- function(n, nbcenters, dim = 5){
    x <- generate_sets(n, nbcenters ,dim)
    m <- matrix(unlist(x), ncol = dim)
    df <- as.data.frame(m)
    return(df)
}

NORM10 <-generate_norms(1000, 10, 5)
NORM25 <-generate_norms(1000, 25, 5)