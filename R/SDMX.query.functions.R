
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Lookup table of concepts within a particular dimension. 
##' @param provider 
##' @param flow 
##' @param concept.dimension 
##' @return data.table with lookup information 
##' @author Janko Cizel
sdmxLookupDim <- function(provider = "IMF",
                          flow = "PGI",
                          concept.dimension = "PGI_CONCEPT"){
    ## cat("###           CREATING A LOOKUP TABLE           ###\n")
    ## cat("PROVIDER: ",provider,"\n")
    ## cat("FLOW: ",flow,"\n")
    codes <- getCodes(provider = provider,
                      flow = flow,
                      dimension = concept.dimension)

    res <- list()
    for (x in names(codes)){
        res[[length(res)+1]] <-
            as.data.table(
                list(NAME = x,
                     LABEL = SDMXWrappers:::.trim(codes[[x]])))
    }

    out <- rbindlist(res, fill = TRUE)
    return(out)
}

## sdmxLookupDim(
##     provider = "IMF",
##     flow = "PGI",
##     concept.dimension  = "PGI_CONCEPT"
## )

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Create a table of concepts within a particular data flow.
##' @param provider 
##' @param flow 
##' @return List of data.tables contining lookups for each dimension within a
##' data flow.
##' @author Janko Cizel
sdmxConceptLookup <- function(provider = "IMF",
                              flow = "PGI"){

    dims <- 
        getDimensions(provider = provider,
                      dataflow = flow)

    res <- list()
    for (x in names(dims)){
        res[[dims[[x]]]] <- sdmxLookupDim(
            provider = provider,
            flow = flow,
            concept.dimension = x)
    }

    return(res)
}


## conc <- 
##     sdmxConceptLookup(provider = "IMF",
##                       flow = "PGI")

## getFlows("IMF")
## conc <- 
## .conceptLookups(provider = "IMF",
##                 flow = "MCORE")

## 


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title DOWNLOAD AND SAVE ALL DATA FLOWS FROM A GIVEN PROVIDER
##' @param provider 
##' @param folder 
##' @return NULL 
##' @author Janko Cizel
sdmxGetAllFlows <- function(provider = "ECB",
                            folder = "/Users/jankocizel/Downloads"){
    flows <-  getFlows(provider = provider)
    flowsm <- lapply(flows, function(str){
        o <- strsplit(str,split = ";")[[1]]
        o <- .trim(o)
        o[1] <- gsub("[[:punct:]]+","_",o[1])
        o[2] <- gsub("[[:space:]]+","_",o[2])
        paste0(o[1],".",o[2])
    })

    ## for (x in names(flowsm)){
    ##     conc <- 
    ##         sdmxConceptLookup(provider = provider,
    ##                           flow = x)
    ##     SDMXWrappers:::.saveExcel(l = conc,
    ##                               file =
    ##                                   paste0(folder,"/",flowsm[[x]],".xlsx"))        
    ## }

    out <- foreach (x = names(flowsm),
                    .inorder = TRUE) %dopar% {
        conc <- 
            SDMXWrappers:::sdmxConceptLookup(provider = provider,
                              flow = x)
    }
    names(out) <- names(flowsm)

    
    return(NULL)
}

## sdmxGetAllFLows(provider = "ECB")
## sdmxGetAllFLows(provider = "IMF")
## sdmxGetAllFLows(provider = "ILO")
## sdmxGetAllFLows(provider = "INEGI")
## sdmxGetAllFLows(provider = "OECD")
## sdmxGetAllFLows(provider = "EUROSTAT")


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Collect Static Information from RJSDMX:::getSDMX call
##' @param o Object returned by the RJSDMX:::getSDMX function
##' @return data.table with static information 
##' @author Janko Cizel
sdmxCollectStaticData <- function(o){
    require(data.table)
    res <- list()
    for (x in names(o)){
        a <- attributes(o[[x]])
        a.m <- lapply(a,function(x) paste(x, collapse = ';'))
        res[[x]] <- do.call(data.table, a.m)
    }

    out <- rbindlist(res, fill = TRUE)
    
    return(out)
}

## lookup <- .collectStaticData(o)
## write.xlsx2(x = lookup,
##             file = "/Users/jankocizel/Downloads/test.xlsx",
##             append = TRUE)

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Collect Time Series  from RJSDMX:::getSDMX call
##' @param o Object returned by the RJSDMX:::getSDMX function
##' @return data.table with time series.
##' @author Janko Cizel
sdmxCollectTSData <- function(o){
    res <- list()
    for (x in names(o)){
        temp <- 
            list(ID = rep(x, times = length(o[[x]])),
                 DATE = index(o[[x]]),
                 VALUE = as.numeric(o[[x]]))
        res[[x]] <- do.call(data.table, temp)
    }

    out <- rbindlist(res, fill = TRUE)
    
    return(out)
}

## out <- .collectTSData(o)

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Pull all data from a given provider, and reshape them into the
##' analysis-ready format.
##' @param provider Data provider, e.g. "ECB"
##' @param folder Destination folder
##' @param flow Data flow to download. If NULL, download all flows for a given provider.
##' @return NULL. The function saves all datasets as excel files in a
##' prespecified folder
##' @author Janko Cizel
sdmxGetAllData <- function(provider = "ECB",
                        folder = "/Users/jankocizel/Downloads",
                        flow = NULL){
    if (is.null(flow)){
        flows <-  getFlows(provider = provider)
    } else {
        flows <- getFlows(provider = provider, pattern = flow)
    }
    
    flowsm <- lapply(flows, function(str){
        o <- strsplit(str,split = ";")[[1]]
        o <- .trim(o)
        o[1] <- gsub("[[:punct:]]+","_",o[1])
        o[2] <- gsub("[[:space:]]+","_",o[2])
        paste0(o[1],".",o[2])
    })

    ## CHECK WHETHER THE FILE ALREADY EXISTS
    .fileExists <- function(file,folder){
        return(file %in% list.files(path = folder))
    }

    for (x in names(flowsm)) {
        cat("### FLOW: ", flows[[x]],"\n")
        cat("\n")

        if (.fileExists(file = paste0(flowsm[[x]],".xlsx"), folder = folder)) {
            cat("File already exists!! \n")
            next
        }
        
        res <- list()
        res <- sdmxConceptLookup(provider = provider,
                                 flow = x)
        
        d <- getDimensions(provider = provider,
                           dataflow = x)
        query <- paste(c(x,rep(".*",times = length(d))), collapse = "")
        cat(query,"\n")

        obj <-
            try(getSDMX(provider = provider,
                        id = query))
        
        res[["TS"]] <- try(sdmxCollectTSData(obj))
        res[["STATIC"]] <- try(sdmxCollectStaticData(obj))
        
        .saveExcel(l = res,
                   file = paste0(folder,"/",flowsm[[x]],".xlsx"))        
    }
}

## .getAllData(provider = "IMF")
## .getAllData(provider = "OECD")
## .getAllData(provider = "ECB")
## .getAllData(provider = "ECB", flow = "EXR")



sdmxGetAllDataParallel <- function(provider = "ECB",
                                   folder = "/Users/jankocizel/Downloads",
                                   flow = NULL){
    if (is.null(flow)){
        flows <-  RJSDMX:::getFlows(provider = provider)
    } else {
        flows <- RJSDMX:::getFlows(provider = provider, pattern = flow)
    }
    
    flowsm <- lapply(flows, function(str){
        o <- strsplit(str,split = ";")[[1]]
        o <- .trim(o)
        o[1] <- gsub("[[:punct:]]+","_",o[1])
        o[2] <- gsub("[[:space:]]+","_",o[2])
        paste0(o[1],".",o[2])
    })

    ## CHECK WHETHER THE FILE ALREADY EXISTS
    .fileExists <- function(file,folder){
        return(file %in% list.files(path = folder))
    }

    out <- 
        foreach (x = names(flowsm)) %dopar%
    {
        cat("### FLOW: ", flows[[x]],"\n")
        cat("\n")

        if (.fileExists(file = paste0(flowsm[[x]],".xlsx"), folder = folder)) {
            cat("File already exists!! \n")
            stop
        }
        
        res <- list()
        res <- SDMXWrappers:::sdmxConceptLookup(provider = provider,
                                                flow = x)
        
        d <- RJSDMX:::getDimensions(provider = provider,
                                    dataflow = x)
        query <- paste(c(x,rep(".*",times = length(d))), collapse = "")
        cat(query,"\n")

        obj <-
            try(RJSDMX:::getSDMX(provider = provider,
                                 id = query))
        
        res[["TS"]] <- try(SDMXWrappers:::sdmxCollectTSData(obj))
        res[["STATIC"]] <- try(SDMXWrappers:::sdmxCollectStaticData(obj))
        
        SDMXWrappers:::.saveExcel(l = res,
                   file = paste0(folder,"/",flowsm[[x]],".xlsx"))
        NULL
    }
}

## sdmxGetAllDataParallel('ECB')
## sdmxGetAllDataParallel('IMF')
