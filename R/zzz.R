.onLoad <- function(...){
    cl <- parallel:::makeCluster(8, outfile = "./log/l1.log")
    doParallel:::registerDoParallel(cl)

    options(java.parameters = "-Xmx4000m")
}
