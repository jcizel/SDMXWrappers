
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
                 DATE = zoo:::index(o[[x]]),
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
                                   datafolder = "inst/extdata",
                                   flow = NULL){
    if (is.null(flow)){
        flows <-  RJSDMX:::getFlows(provider = provider)
    } else {
        flows <- RJSDMX:::getFlows(provider = provider, pattern = flow)
    }
    
    flowsm <- lapply(flows, function(str){
        o <- strsplit(str,split = ";")[[1]]
        o <- SDMXWrappers:::.trim(o)
        o[1] <- gsub("[[:punct:]]+","_",o[1])
        o[2] <- gsub("[[:space:]]+","_",o[2])
        paste0(o[1],".",o[2])
    })

    ## CHECK WHETHER THE FILE ALREADY EXISTS
    .fileExists <- function(file,folder){
        return(file %in% list.files(path = folder))
    }

    folder <- paste0(datafolder,'/',provider)
    if (!.fileExists(file = provider, folder = paste0(datafolder))){
        system(command = paste0('mkdir ',folder))
    }

    
    out <- 
        foreach (x = names(flowsm),
                 .errorhandling = 'pass') %do%
    {
        options(java.parameters = "-Xmx8g")
        cat("### FLOW: ", flows[[x]],"\n")
        cat("\n")

        folder2 <- paste0(datafolder,'/',provider,'/',x)
        
        if (!.fileExists(file = x, folder = folder)){
            system(command = paste0('mkdir ',folder2))
        }

        flist <- list.files(path = folder2, full.names = TRUE)

        if (length(flist) > 0){
            if (sum(sapply(flist, function(x) {
                o <- file.info(x)$size/(2^20)
                if (is.na(o)) o <- 0
                o
            }))>1){
                stop("File already exists.")
            }
        }
        
        res <- list()
        res <- SDMXWrappers:::sdmxConceptLookup(provider = provider,
                                                flow = x)
        
        d <- RJSDMX:::getDimensions(provider = provider,
                                    dataflow = x)
        query <- paste(c(x,rep(".*",times = length(d))), collapse = "")
        cat(query,"\n")


        obj <-
            RJSDMX:::getSDMX(provider = provider,
                             id = query)
        
        res[["TS"]] <- SDMXWrappers:::sdmxCollectTSData(obj)
        res[["STATIC"]] <- SDMXWrappers:::sdmxCollectStaticData(obj)

        f <- paste0(folder2,"/",flowsm[[x]])
        for (y in names(res)){
            o <- res[[y]]
            print(str(o,1))
            y <- gsub('[[:punct:]]+','.',y)
            write.csv(x = o,
                      file = paste0(f,"---",y,".csv"))
            cat('Written to file: ', paste0(f,"---",y,".csv"),'\n')
        }

        res[['STATIC']]
    }
}

## out <- sdmxGetAllDataParallel('ECB')
## out <- sdmxGetAllDataParallel('IMF')
## sdmxGetAllDataParallel('IMF')


getListOfVariables <- function(
    provider = 'ECB',
    outfile = paste0('inst/extdata/',provider,'-VariableList.csv')
){
    .f <- list.files(paste0('inst/extdata/',provider),
                     all.files = TRUE,
                     full.names = TRUE,
                     recursive = TRUE,
                     pattern = "STATIC")

    o <-
        foreach( x = .f,
                .errorhandling = 'remove'
                ) %dopar% {
            d <- data.table:::fread(x)
            index <- d$index

            time <- 
                lapply(index, function(ix) {
                    o <- stringr:::str_split(ix,
                                             pattern = ";")
                    out <- stringr:::str_extract(o[[1]],
                                                 pattern = "[[:digit:]]{4}")
                    return(list(yearmin = min(as.numeric(out)),
                                yearmax = max(as.numeric(out))))
                })
            
            d[, list(ID,
                     TITLE_COMPL,
                     FREQ = frequency,
                     UNIT,
                     UNIT_MULT,
                     LENGTH = nchar(index),
                     YEARMIN = sapply(time,function(x) x$yearmin),
                     YEARMAX = sapply(time,function(x) x$yearmax))]
        }
    
    out <- rbindlist(o)

    out[, db := {
        x <- stringr:::str_split(ID,
                                 pattern = '\\.')
        sapply(x, function(l) l[[1]])
    }]

    if (!is.null(outfile)) write.csv(out, file = outfile)

    return(out)
}


getCoreDataset <- function(
    provider = 'ECB',
    outfile = paste0('inst/extdata/',provider,'-TS.csv')
){
    .f <- list.files(paste0('inst/extdata/',provider),
                     all.files = TRUE,
                     full.names = TRUE,
                     recursive = TRUE,
                     pattern = "TS\\.csv")

    o <-
        foreach( x = .f,
                .errorhandling = 'remove'
                ) %dopar% {
            d <- data.table:::fread(x)
            
            d
        }
    
    out <- rbindlist(o, fill = TRUE)

    if (!is.null(outfile)) write.csv(out, file = outfile)

    return(out)
}

## ts <- getCoreDataset(provider = 'ECB')

queryVariableList <- function(pattern = "",
                              provider = 'ECB') {
    .files <- list.files('./inst/extdata')
    if (paste0(provider,'-VariableList.csv') %in% .files){
        v <- fread(input = paste0('inst/extdata/',provider,'-VariableList.csv'))
    } else {
        v <- getListOfVariables(provider = 'ECB')
    }

    out <- v[toupper(TITLE_COMPL) %like% toupper(pattern)]

    return(out)
}

## queryVariableList('yield')[UNIT %like% "PCPA"][FREQ==1][toupper(TITLE_COMPL) %like% "ZERO"]
## queryVariableList('probability')
## queryVariableList('loan')
## queryVariableList('default')$TITLE_COMPL
## queryVariableList('counterparty')
## queryVariableList('issuance')
## queryVariableList('covered')
## queryVariableList('government')[YEARMIN<1990]
## queryVariableList('external debt')
## queryVariableList('supply')

## varlist <- getListOfVariables('ECB')
## varlist[grepl("ST1.Q.[A-Z]+.N.8.990.N.A1.E",ID, perl = TRUE)]

## varlist[grepl("RPV.A.[A-Z]+.N.TD.00.3.AV",ID, perl = TRUE)]
## varlist[grepl("RPP.H.[A-Z]+.N.ED.00",ID, perl = TRUE)]
## ts[grepl("RPP.H.[A-Z]+.N.ED.00",ID, perl = TRUE)]

