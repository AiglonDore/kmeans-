phi <- function(points, centroids){
    s <- 0
    for (p in points){
        norms <- list()
        for (c in centroids){
            norms <- append(norms, norm(p - c, type = "2") * norm(p - c, type = "2"))
        }
        s <- s + min(unlist(norms))
    }
    return(s)
}

weight <- phi