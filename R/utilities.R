.trim <- function(str) gsub("^\\s+|\\s+$", "", str)


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Save a list of data frames as excel file (each table in its sheet)
##' @param l 
##' @param file 
##' @return NULL
##' @author Janko Cizel
.saveExcel <- function(l,
                       file){
    names(l) <- gsub("[[:punct:]]+","_",names(l))
    
    for (x in names(l)){
        ## IF NUMBER OF ROWS EXCEEDS 1000000, EXCEL SHEET CANNOT BE WRITTEN
        if (nrow(l[[x]]) > 1e6){
            write.csv(x = l[[x]],
                      file = paste0(file,".",x,".csv"))
        } else {
            write.xlsx2(x = l[[x]],
                        file = file,
                        sheetName = x,
                        append = TRUE)
        }
    }
}




.lookupName <- function(query,
                        lookup.table,
                        id.var = 'REF_AREA',
                        label.var = 'LABEL',
                        label.len = 20){
    setkeyv(lookup.table, id.var)
    out <- paste(lookup.table[query, get(label.var)][[1]],
                 collapse = ';')
    
    return(out)
}

## .lookupName(query = 'GST.A.SI.N.B0000.CUR.B1300.CU.G',
##             lookup.table = lookup,
##             label.len = 50)

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Read XLSX file that results from the call to
##' SDMXWrappers:::sdmxGetAllData function
##' @param file Path to the xlsx file
##' @return List of data.tables. Each list entry contains data from a specific
##' excel sheet
##' @author Janko Cizel
readXlsxSheets <- function(file){
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

## l <- read.xlsx.sheets(file =
## paste0(PATH.TEMP,"/EUROSTAT_IEAF_1_0.Quarterly_non-financial_accounts,_QSA_by_country.xlsx"))


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

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Create a reshaped dataset from the list obtained from
##' SDMXWrappers:::readXlsxSheets call.
##' @param sheetList List object obtained from SDMXWrappers:::readXlsxSheets call
##' @param colVar Name of variable that should be used as a column in a reshaped dataset.
##' @param id ID variable, typically ISO3 country code
##' @param time  time variable, typically date.
##' @return data.table with additional attributes containing labeling information. 
##' @author Janko Cizel
createDataSet <- function(sheetList,
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
