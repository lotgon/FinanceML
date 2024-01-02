#series: data.table with value column
TranformToDropdawnData<-function(series, columnName="value"){
#  series_ids <- series[,unique(series_id)]
  suppressWarnings({series[,maxValue:=purrr::accumulate(get(columnName), max, na.rm=T), by=series_id]})
  series[,dropdawn:=get(columnName)/maxValue, by=series_id]
  series[,dropdawnMVA:=frollmean(dropdawn, 7), by=series_id]
  series[dropdawnMVA==-Inf, dropdawnMVA:=NA]
  series
}
TranformTo_LastMax <- function(series, threshold=0.9, columnName="value"){
  #in parameters
  #quotes as data.table[series_id, value] ordered by time
  for(sym in series[,unique(series_id)])
    TranformTo_LastMaxPerSeriesId(series, sym, threshold, columnName)
  series
}
TranformTo_LastMaxPerSeriesId <- function(quotes, chosen_series_id, threshold, columnName="value"){
  #in parameters
  #quotes as data.table[series_id, value] ordered by time
  cPrevMax <- numeric(quotes[series_id==chosen_series_id, .N])
  i <- 0;prevMax_ <- NA;currentMax <- 0;isTrendUp <- TRUE
  for(price in quotes[series_id==chosen_series_id,get(columnName)]){
    i<-i+1
    if(!is.na(price)){      
      if( price > currentMax){
        currentMax <- price
        isTrendUp <- TRUE
      }
      
      if(isTrendUp==TRUE & price < threshold*currentMax){
        isTrendUp <- FALSE
        prevMax_ <- currentMax
      }
    }
    cPrevMax[i] <- prevMax_
  }
  quotes[series_id==chosen_series_id, `:=`(prevMax=cPrevMax)]
  quotes[series_id==chosen_series_id, growthIndex:=maxValue / prevMax*dropdawn]
}

Scale_Max <- function(num_data, max_value){
  return( num_data*max_value/max(num_data, na.rm = T) )
}
