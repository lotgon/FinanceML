require(ggplot2)
require(data.table)

#input data.table with 2 columns with column "price"
#output data.table with additional table
#return potential profit for investment with Buy&Movement Strategy
#reqPrices on required price levels should be opened reqVolumes. If price is lower than reqPrices[1] than hold position
EmulateInvestment <- function(data, reqPrices, reqVolumes){
  
  data[,backtestPrice:=0]
  data[,backtestVolume:=0]
  data[,backtestProfit:=0]
  data[,backtestSwaps:=0]

  reqIntervalPrices <- c(-Inf, reqPrices, Inf)
  pricesInterval <- cut(data$price, reqIntervalPrices, labels = FALSE)
  currVolume <- 0
  lastInterval <- pricesInterval[1]

  for(i in 1:(length(pricesInterval)-1)){
    if(currVolume != 0 & i>1){
      diffDays <- data[i, date] - data[i-1, date]
      data[i, backtestSwaps:= diffDays * currVolume]
    }
    #should buy if necessary
    if(pricesInterval[i] < lastInterval ){
      reqVolume <- reqVolumes[pricesInterval[i]]
      if( currVolume < reqVolume ){
        data[i, backtestPrice:=reqPrices[pricesInterval[i]]]
        data[i, backtestVolume:=reqVolume - currVolume]
        data[i, backtestProfit:=0]
        currVolume = reqVolume
      }
    }
    #should sell if necessary
    if(pricesInterval[i] > lastInterval ){
      reqVolume <- reqVolumes[pricesInterval[i]-1]
      if( currVolume > reqVolume ){
        data[i, backtestPrice:=reqPrices[pricesInterval[i]-1]]
        data[i, backtestVolume:=reqVolume - currVolume]
        data[i, backtestProfit:=(backtestPrice-reqPrices[pricesInterval[i]-2]) * -backtestVolume]
        currVolume = reqVolume
      }
    }
    lastInterval = pricesInterval[i]
  }

  data[.N, backtestVolume:=-currVolume]
  data[.N, backtestPrice:=price]
  vwapTotalBuy <- data[backtestVolume>0, sum(backtestVolume*backtestPrice)/sum(backtestVolume)]
  volumeTotalBuy <- data[backtestVolume>0, sum(backtestVolume)]
  vwapTotalSell <- data[backtestVolume<0, sum(backtestVolume*backtestPrice)/sum(backtestVolume)]
  volumeTotalSell <- data[backtestVolume>0, -sum(backtestVolume)]
  totalProfit <- (vwapTotalSell - vwapTotalBuy) * volumeTotalBuy
  data[.N, backtestProfit:=totalProfit - data[,sum(backtestProfit)]]
  return(data)
}

EmulateInvestment <- function(){
  prices <- c(51, 50:150, 150:1)
  reqPrices <- seq(50, 100, 10)
  reqVolumes <- seq(6, 1, -1)
  resTest1 <- EmulateInvestment(data.table(date=1:length(prices), price=prices), reqPrices, reqVolumes)

  ggplot(resTest1, aes(x=date, y=price)) + geom_line() +
    geom_point(data = resTest1[backtestVolume>0], mapping  = aes(x=date, y=price, colour='buy')) +
    geom_point(data = resTest1[backtestVolume<0], mapping  = aes(x=date, y=price, colour='sell'))   
}


PriceMov_Reduce <- function(prices, reqPrices, reqVolumes){
  #addedVolumes <- c(reqVolumes[1], diff(reqVolumes))
  breaks = c(0, reqPrices, Inf)
  activation <- rle(cut(prices, breaks, labels = FALSE))$values
  profitPips <- 0
  #profit <- 0
  if( length(activation) > 2 )
    for( i in seq(3, length(activation), 1))
      if( activation[i]-2  >= activation[i-2]){
        profitPips <- profitPips + reqPrices[activation[i-1]] -  reqPrices[activation[i-2]]
        #profit <- profit + addedVolumes[activation[i-1]]
      }
  minInterval <- min(activation)
  floatingProfit <- prices[length(prices)] - reqPrices[minInterval:length(reqPrices)]
  return(profitPips+sum(floatingProfit))
}
PriceMov_Reduce_test <- function(){
  prices <- c(50:150, 150:1)
  reqPrices <- seq(10, 100, 10)
  reqVolumes <- seq(1, 10, 1)
  resTest1 <- PriceMov_Reduce(prices, reqPrices)
  prices <- c(1:150, 150:50)
  reqPrices <- seq(10, 100, 10)
  resTest1 <- PriceMov_Reduce(prices, reqPrices)
  (resTest1)
}
