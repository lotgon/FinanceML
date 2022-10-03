#return always daily data
localStorage_getSeries <- function(series_id, startDate=ISOdate(1990, 1, 1, tz = "UTC"), endDate=ISOdate(2020, 1, 1, tz = "UTC")){
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  d <- fread(paste0("..\\data\\", series_id))#, colClasses = c("character", "numeric")
  colnames(d) <- c("date", "value")
  period <- data.table(date = seq(startDate, endDate, 'days' ))
  d <- d[period, on=.(date), roll=TRUE]
  d[, series_id:=series_id]
  return(d)
}
