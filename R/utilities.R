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
