require(devtools)
load_all()

require(GeneralUtilities)
require(rlist)
require(pipeR)

flows <-
    getFlows(provider = 'OECD')

flows2 <- 
    flows %>>%
    list.map(. %>>%
         stringr::str_split(';') %>>%
         unlist %>>%
         stringr::str_trim() %>>%
         as.list %>>%
         as.data.table) %>>% rbindlist

flows2[, FLOW := {
    V1 %>>%
    stringr::str_split(',') %>>%
    list.map(.[[2]]) %>>%
    unlist    
}] %>>% write.csv('~/Downloads/test.csv')


## ---------------------------------------------------------------------------- ##
## PULL OECD DATA TABLES                                                        ##
##                                                                              ##
## (THE EASIEST WAY IS TO BROWSE THROUGH THE TABLES ON THE OECD WEBPAGE, SELECT ##
## THE TABLES OF INTEREST, AND PULL THEM VIA THIS API)                          ##
## ---------------------------------------------------------------------------- ##

BPF1 <- prepareOECDData(provider = 'OECD',
                     dataflow = 'BPF1',
                     iso.pos = 4)


GOV_DEBT <- prepareOECDData(provider = 'OECD',
                            dataflow = 'GOV_DEBT',
                            iso.pos = 2)


## REVENUE STATISTICS - LATIN AMERICA
RSLACT <- prepareOECDData(provider = 'OECD',
                          dataflow = 'RSLACT',
                          iso.pos = 2)


## REVENUE STATISTICS - OECD
REV <- prepareOECDData(provider = 'OECD',
                       dataflow = 'REV',
                       iso.pos = 5)

## National Accounts at a Glance - NAAG
## debug(prepareOECDData)
NAAG <- prepareOECDData(provider = 'OECD',
                        dataflow = 'NAAG',
                        iso.pos = 2,
                        drop.cols = c('ID','LOCATION'))


## STAN Database for Structural Analysis
## BTDIXE_I4 <- prepareOECDData(provider = 'OECD',
##                              dataflow = 'BTDIXE_I4',
##                              iso.pos = NULL)


SNA_TABLE12 <- prepareOECDData(provider = 'OECD',
                               dataflow = 'SNA_TABLE12',
                               iso.pos = 2,
                               drop.cols = c('ID','LOCATION'))


SNA_TABLE610 <- prepareOECDData(provider = 'OECD',
                                dataflow = 'SNA_TABLE610',
                                iso.pos = NULL,
                                drop.cols = c('ID','LOCATION'))

## COMPILE THE GENERAL LOOKUP TABLE

l <- getLookupsFromExistingCSV(
    provider = 'OECD',
    pattern = 'LOOKUPTABLE\\.csv',
    outfile = './inst/extdata/OECD-VariableList.csv'
)



