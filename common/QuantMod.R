library(tidyquant)
require(data.table)

#getSymbols("^SPX", from = '2008-01-01', to = "2008-03-01",warnings = FALSE)
#d <- getSymbols('^GSPC', from = 1980-01-01, auto.assign = FALSE) 

#^GSPC
qmod_getSeries <- function(series_id, startDate=ISOdate(1990, 1, 1, tz = "UTC"), endDate=ISOdate(2020, 1, 1, tz = "UTC")){
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  
  qdFileName <- paste0("..//.cache//", series_id, " ", format(startDate, "%Y%m%d"), "  ", format(endDate, "%Y%m%d"), ".csv")
  if( !file.exists(qdFileName) ){
    d <- getSymbols(series_id, from = startDate, to=endDate, auto.assign = FALSE) 
    
    d <- as.data.table(d)
    colnames(d) <- c('date', 'open', 'high', 'low', 'close', 'volume', 'adjusted')
    d[,value:=(high+low)/2]
    d <- d[, .(date, value)]
    period <- data.table(date = seq(startDate, endDate, 'days' ))
    d <- d[period, on=.(date), roll=TRUE]
    d[, series_id:=series_id]
    d[,date:=as.Date(date)]
    fwrite(d, qdFileName)
  }
  d <- fread(qdFileName, col.names = c("date", "value", "series_id"))
  #remove after fix https://github.com/Rdatatable/data.table/issues/5309
  d[,date:=as.Date(date)]
  return(d)
}
