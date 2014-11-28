.onLoad <- function(...){
    cl <- parallel:::makeCluster(8)
    doParallel:::registerDoParallel(cl)

    options(java.parameters = "-Xmx4000m")
}
