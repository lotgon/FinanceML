DailyProfitRatio_single <- function(symbol, year, dayStart, dayEnd, isBuy){
  d <- GetQuotes(symbol, startDate=ISOdate(year, 1, 1, tz = "UTC"), endDate=ISOdate(year+1, 1, 1, tz = "UTC"), periodicity="D1", type="Bids")
  if( nrow(d)==0)
    return(NA)
  d[,day := lubridate::yday(datetime)]
  setkey(d, day)
  d <- d[CJ(1:366)]
  setnafill(d, type="locf")

  ratio <- d[dayEnd,  (high+low)/2] / d[dayStart,  (high+low)/2]
  
  if( isBuy )
    ratio
  else
    1/ratio
}
DailyProfitRatio <- function(symbols, years, dayStarts, dayEnds, isBuys){
  unlist(Map(DailyProfitRatio_single, symbols, years, dayStarts, dayEnds, isBuys))
}
