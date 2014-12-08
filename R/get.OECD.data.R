## obj <-
##     try(getSDMX(provider = 'OECD',
##                 id = query))
## res <- SDMXWrappers:::sdmxConceptLookup(provider = 'OECD',
##                                         flow = "GOV_DEBT")

## dt <- createOECDDatasetFromRequest(obj)
## l <- createStaticOECDDatasetFromRequest(obj,res)
createOECDDatasetFromRequest <- function(obj,
                                         iso.pos = 2){
    obj %>>%
    list.map(
        data.table(
            ID = .name,
            date = GeneralUtilities::convertToDate(zoo::index(.)),
            value = as.numeric(.)
        )
    ) %>>%
    rbindlist  %>>%
    (~ dt)

    dt[, c('iso3','varcode') := {
        ID %>>%
        stringr::str_split('\\.') %>>%
        list.map(. %>>% as.list) %>>%
        (~ temp)

        list(iso3 = temp %>>% list.map(.[[iso.pos]]) %>>% unlist,
             varcode =
                 temp %>>% list.map(. %>>%
                                    list.remove(c(1,iso.pos)) %>>%
                                    paste(collapse = '.')) %>>% unlist)    
    }]

    dt %>>%
    data.table::dcast.data.table(
        formula = iso3 + date ~ varcode,
        value.var = 'value'
    ) %>>%
    (~out) %>>%
    invisible

    return(out)
}



createStaticOECDDatasetFromRequest <- function(obj,
                                               res,
                                               iso.pos = 2,
                                               drop.cols = c('ID','COU')){
    .lookupAttribute <- function(dim,
                                 value, 
                                 res){
        lookup <- try(res %>>% list.match(dim) %>>% (l ~ l[[1]]),
                      silent = TRUE)

        if (inherits(lookup,'try-error')) return(value)
        
        setkey(lookup, NAME)

        out <- lookup[value] %>>% (LABEL) %>>% paste(collapse = ';')

        return(out)
    }


    obj %>>%
    list.map(. %>>% attributes %>>% list.remove(c('index','frequency','class','STATUS'))) %>>%
    list.map(. %>>% list.map(.lookupAttribute(dim = .name,
                                              value = .,
                                              res = res))) %>>%
    list.map(. %>>% as.data.table) %>>%
    rbindlist(fill = TRUE) %>>%
    (~ lookup)


    lookup[, IDnew := {
        ID %>>%
        stringr::str_split('\\.') %>>%
        list.map(. %>>%
                 list.remove(c(1,iso.pos)) %>>%
                 paste(collapse = '.')) %>>%
        unlist
    }] %>>%
    (dt ~ dt[, .SD, .SDcols = -drop.cols]) %>>%
    unique %>>%
    (~ out)

    return(out)
}



.fileExists <- function(file,folder){
    return(file %in% list.files(path = folder))
}

downloadOECDData <- function(provider = 'OECD',
                             dataflow = 'GOV_DEBT',
                             datafolder = './inst/extdata'){

    ## ---------------------------------------------------------------------- ##
    ## PRELIMINARIES                                                          ##
    ## ---------------------------------------------------------------------- ##
    
    folder <- paste0(datafolder,'/',provider,'/',dataflow)
    
    system(command = paste0('mkdir ',folder))


    flist <- list.files(path = folder, full.names = TRUE)

    if (length(flist) > 0){
        if (sum(sapply(flist, function(x) {
            o <- file.info(x)$size/(2^20)
            if (is.na(o)) o <- 0
            o
        }))>1){
            stop("File already exists.")
        }
    }

    ## ---------------------------------------------------------------------- ##
    ## MAIN                                                                   ##
    ## ---------------------------------------------------------------------- ##
    d <- getDimensions(provider = provider,
                       dataflow = dataflow)

    query <- paste(
        c(dataflow,
          rep(".*",times = length(d))),
        collapse = "")
    cat(query,"\n")

    obj <-
        try(getSDMX(provider = provider,
                    id = query))

    if (inherits(obj,'try-error')){
        ## Check why!
        obj <-
            try(getSDMX(provider = provider,
                        id = paste0(query,'.*')))
    }

    res <- SDMXWrappers:::sdmxConceptLookup(provider = provider,
                                            flow = dataflow)


    ## ---------------------------------------------------------------------- ##
    ## OUTPUT                                                                 ##
    ## ---------------------------------------------------------------------- ##
    out <-
        list(obj = obj,
             res = res)

    outfile <- sprintf("%s/%s/%s/WebDownload.RData",
                       datafolder,
                       provider,
                       dataflow)
    
    out %>>% list.save(file = outfile)

    cat(sprintf('Downloaded file written to %s.\n',outfile))
    
    return(out)
}


prepareOECDData <- function(provider = 'OECD',
                            dataflow = 'GOV_DEBT',
                            iso.pos = NULL,
                            drop.cols = c('ID','COU')){

    path <- sprintf("%s/%s/%s",
                    datafolder,
                    provider,
                    dataflow)

    if ('WebDownload.RData' %in% list.files(path = path, full.names = FALSE)){
        list.load(paste0(path,'/WebDownload.Rdata')) %>>% (~ r) %>>% invisible
    } else {
        r <- downloadOECDData(provider = provider,
                              dataflow = dataflow)
    }

    cat(names(r$obj)[1:10],'\n')

    if (is.null(iso.pos)) stop('Specify the position if country identifier within the ID string!')

    dt <- createOECDDatasetFromRequest(r$obj,iso.pos = iso.pos)
    browser()
    l <- createStaticOECDDatasetFromRequest(r$obj,
                                            r$res,
                                            iso.pos = iso.pos,
                                            drop.cols = drop.cols)

    l[['LABEL']] <-  l %>>% as.list %>>% list.remove('IDnew') %>>%
    (li ~ do.call('sprintf', c(li, fmt = rep('%s',times = length(li)) %>>% paste(collapse ='; ')))) 

    dt %>>% write.csv(file = paste0(path,'/DATATABLE.csv'))
    l %>>% write.csv(file = paste0(path,'/LOOKUPTABLE.csv'))

    out <-
        structure(dt,
                  lookup = l)

    return(out)
}

## USEFUL DATAFLOWS:
## GOV_DEBT,
## BPF1


