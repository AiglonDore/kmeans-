#Step 1
kmeans_init <- function(d,k){
    #d is the dimension
    #k is the number of clusters
    output = list(floor(runif(n = d, min = 0, max = 100)))
    for (i in 2:k)
    {
        x <- floor(runif(n = d, min = 0, max = 100));
        x <- list(x)
        output <- append(output, x)
    }
    return(output)
}

#Step 2
