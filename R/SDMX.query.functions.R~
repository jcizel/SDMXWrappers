
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param provider 
##' @param flow 
##' @param concept.dimension 
##' @return 
##' @author Janko Cizel
sdmxLookupDim <- function(provider = "IMF",
                          flow = "PGI",
                          concept.dimension = "PGI_CONCEPT"){
    cat("###           CREATING A LOOKUP TABLE           ###\n")
    cat("PROVIDER: ",provider,"\n")
    cat("FLOW: ",flow,"\n")
    codes <- getCodes(provider = provider,
                      flow = flow,
                      dimension = concept.dimension)

    res <- list()
    for (x in names(codes)){
        res[[length(res)+1]] <-
            list(NAME = x,
                 LABEL = codes[[x]])
    }

    res.df <- do.call(rbind,res)
    return(res.df)
}

## sdmxVariablesInFlow(
##     provider = "IMF",
##     flow = "PGI",
##     concept.dimension  = "PGI_CONCEPT"
## )

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param provider 
##' @param flow 
##' @return 
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
##     .conceptLookups(provider = "IMF",
##                     flow = "PGI")

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

    for (x in names(flowsm)){
        conc <- 
            .conceptLookups(provider = provider,
                            flow = x)
        SDMXWrappers:::.saveExcel(l = conc,
                                  file =
                                      paste0(folder,"/",flowsm[[x]],".xlsx"))        
    }
    return(NULL)
}

## .getAllFlows(provider = "ECB")
## .getAllFlows(provider = "IMF")
## .getAllFlows(provider = "ILO")
## .getAllFlows(provider = "INEGI")
## .getAllFlows(provider = "OECD")
## .getAllFlows(provider = "EUROSTAT")

## 'o' is the object returned by getSDMX function
.collectStaticData <- function(o){
    require(data.table)
    res <- list()
    for (x in names(o)){
        a <- attributes(o[[x]])
        a.m <- lapply(a,function(x) paste(x, collapse = ';'))
        res[[x]] <- do.call(data.table, a.m)
    }
    ## cols <- Reduce(f=intersect,lapply(res, names))
    ## res2 <- lapply(res, function(l) l[cols])
    return(rbindlist(res, fill = TRUE))
}

## lookup <- .collectStaticData(o)
## write.xlsx2(x = lookup,
##             file = "/Users/jankocizel/Downloads/test.xlsx",
##             append = TRUE)

.collectTSData <- function(o){
    res <- list()
    for (x in names(o)){
        temp <- 
            list(ID = rep(x, times = length(o[[x]])),
                 DATE = index(o[[x]]),
                 VALUE = as.numeric(o[[x]]))
        res[[x]] <- do.call(data.table, temp)
    }
    return(do.call("rbind", res))
}

## out <- .collectTSData(o)

.getAllData <- function(provider = "ECB",
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
        res <- .conceptLookups(provider = provider,
                               flow = x)
        
        
        d <- getDimensions(provider = provider,
                           dataflow = x)
        query <- paste(c(x,rep(".*",times = length(d))), collapse = "")
        cat(query,"\n")

        obj <-
            try(getSDMX(provider = provider,
                        id = query))
        
        res[["TS"]] <- try(.collectTSData(obj))
        res[["STATIC"]] <- try(.collectStaticData(obj))
        
        .saveExcel(l = res,
                   file = paste0(folder,"/",flowsm[[x]],".xlsx"))        
    }
}

## .getAllData(provider = "IMF")
## .getAllData(provider = "OECD")
## .getAllData(provider = "ECB")
## .getAllData(provider = "ECB", flow = "EXR")

.lookupName <- function(query,
                        lookup.table,
                        id.var = 'REF_AREA',
                        label.var = 'LABEL',
                        label.len = 20){
    setkeyv(lookup.table, id.var)
    out <- paste(lookup.table[query, get(label.var)][[1]],
                 collapse = ';')

    ## ADD LINE BREAK TO EACH N-TH CHARACTER IN LABEL STRING
    ## n = label.len
    ## r = nchar(out)/n
    
    ## out.m <- 
    ##     paste(read.fwf(textConnection(out),
    ##                    rep(n,times=r), as.is = TRUE),
    ##           collapse = "-\n")

    out.m <- out
    
    return(out.m)
}

## .lookupName(query = 'GST.A.SI.N.B0000.CUR.B1300.CU.G',
##             lookup.table = lookup,
##             label.len = 50)

read.xlsx.sheets <- function(file){
    require(data.table)
    
    ## CREATE A TEMPORARY DIRECTORY
    DIR <- tempdir()
    setwd(dir = DIR)
    
    system(command =
               paste0('xlsx2csv --all ',
                      file,
                      ' ',
                      DIR,
                      '/'))

    sheets <- list.files(path = paste0(DIR))

    res <- list()
    for (x in sheets){
        cat("Read sheet: ", x, "\n")
        res[[x]] <-
            fread(input = x,
                  header = TRUE
                  )
    }

    if (!any(grepl("TS",x = sheets))){
        cat("## NOTE: TS.csv IS LOADED FROM THE SEPARATE FILE!!\n")
        res[["TS.csv"]] <- fread(input = paste0(file,".TS.csv"))
    }

    names(res) <- gsub("\\.csv","",names(res))
    
    return(res)
}

## l <- read.xlsx.sheets(file = paste0(PATH.TEMP,"/EUROSTAT_IEAF_1_0.Quarterly_non-financial_accounts,_QSA_by_country.xlsx"))


.shortCols <- function(df, threshold = 10){
    o <- names(Filter(function(x) max(nchar(x))<threshold, x = df))
    return(o)
}

.examineStatic <- function(dt,
                           col,
                           lookup.table){
    .lookupNameV <- Vectorize(.lookupName, "query")
    res <- list()
    for (x in col){
        o <- dt[, table(get(x))]
        t1 <- as.data.table(o)
        t1[, LABEL := .lookupNameV(V1, lookup.table = lookup.table, id.var = 'NAME')]
        res[[x]] <- t1[order(N, decreasing = T)]
    }    
    return(res)
}


.createDataSet <- function(sheetList,
                           colVar,
                           id,
                           time){
    static <- sheetList$STATIC[,.SD,.SDcols=-c("V1","class")]
    static <- static[, unique(c("ID",.shortCols(static,30))), with = FALSE]
    ts <- sheetList$TS[,.SD,.SDcols=-c("V1")]

    o <- merge(x = static,
               y = ts,
               by = "ID")

    rowVar <- c(id, time, setdiff(names(static),c("ID",colVar,id)))
    formula <-
        paste0(
            paste(rowVar, collapse = " + "),
            " ~ ",
            paste(colVar, collapse = " + ")
        )

    out <- 
        dcast.data.table(data = o,
                         formula = formula,
                         value.var = "VALUE")

    attributes(out)$labels <- local({
        ## sheetList[grepl(pattern = colVar, x = names(sheetList))][[1]]
        l <- sheetList[1:(length(sheetList)-2)]
        rbindlist(l)
    })

    attributes(out)$summary <- local({
        .examineStatic(dt = out,
                       col = rowVar,
                       lookup.table = attributes(out)$labels)
    })

    attributes(out)$colnames <- local({
        .lookupNameV <- Vectorize(.lookupName, "query")
        as.data.frame(.lookupNameV(names(out),lookup.table = attr(out,'labels'), id.var = 'NAME')
                      )
    })

    return(out)
}
