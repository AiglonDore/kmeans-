phi <- function(points, centroids){
    s <- 0
    for (i in nrow(points)){
        norms <- list()
        for (j in nrow(centroids)){
            norms <- append(norms, norm(points[i, ] - centroids[j, ], type = "2") ^ 2)
        }
        s <- s + min(unlist(norms))
    }
    return(s)
}

weight <- phi