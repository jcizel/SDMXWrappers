.onLoad <- function(...){
    cl <- parallel:::makeCluster(8)
    doParallel:::registerDoParallel(cl)
}
