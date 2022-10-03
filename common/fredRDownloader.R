require(data.table)
require(fredr)

#return always daily data
fred_getSeries <- function(series_id, startDate=ISOdate(1990, 1, 1, tz = "UTC"), endDate=ISOdate(2020, 1, 1, tz = "UTC")){
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  
  qdFileName <- paste0("..//.cache//", series_id, " ", format(startDate, "%Y%m%d"), "  ", format(endDate, "%Y%m%d"), ".csv")
  if( !file.exists(qdFileName) ){
    key <- fread("..//common//keys//keys.csv")[name=="fred", key]
    fredr_set_key(key)
    d <- fredr(series_id, observation_start = startDate, observation_end = endDate)
    d <- as.data.table(d)
    d <- d[, .(date, value)]
    period <- data.table(date = seq(startDate, endDate, 'days' ))
    d <- d[period, on=.(date), roll=TRUE]
    d[, series_id:=series_id]
    fwrite(d, qdFileName)
  }
  d <- fread(qdFileName, col.names = c("date", "value", "series_id"))
  return(d)
}


GetFrequency <- function(series_id){
  t<- as.data.table(fredr_series_tags(series_id))
  stringValue <- t[group_id=='freq', name]
  dict = c('monthly' = 'm', 'daily'='d')
  return(dict[stringValue])
}