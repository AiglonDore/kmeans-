phi <- function(points, centroids){
    s <- 0
    for (i in nrow(points)){
        norms <- c()
        for (j in nrow(centroids)){
            norms <- append(norms, norm(points[i, ] - centroids[j, ], type = "2") ** 2)
        }
        s <- s + min(norms)
    }
    return(s)
}