#return always daily data
localStorage_getSeries <- function(series_id, startDate=ISOdate(1990, 1, 1, tz = "UTC"), endDate=ISOdate(2020, 1, 1, tz = "UTC")){
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  d <- fread(paste0("..\\data\\", series_id))#, colClasses = c("character", "numeric")
  colnames(d) <- c("date", "value")
  period <- data.table(date = seq(startDate, d[.N, date], 'days' ))
  d <- d[period, on=.(date), roll=TRUE]
  d[, series_id:=series_id]
  return(d)
}

#tempconvertor to Date,"Closing Value"
TempConvertor <- function(){
  require(data.table)
  require(lubridate)
  d <- fread(paste0("..\\data\\raw\\investing\\", "CBOE Volatility Index Historical Data.csv"))
  d2 <- fread(paste0("..\\data\\raw\\investing\\", "CBOE Volatility Index Historical Data (1).csv"))
  d3 <- fread(paste0("..\\data\\raw\\investing\\", "US Dollar Index Historical Data (2).csv"))
  d <- rbind(d, d2)
  d <- unique(d, by="Date")
  d[,Date:=parse_date_time(Date, 'mdy')]
  d<- d[order(Date)]
  fwrite(d[,.(Date=as.Date(Date), `"Closing Value"`=Price)], paste0("..\\data\\raw\\investing\\", "VIX"))
}