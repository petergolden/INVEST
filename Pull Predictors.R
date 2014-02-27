#INVEST TEAM 
#PREDICT 412
#Code to pull predictors and place into a DF for use

loadPredictorObservations <- function(){
  
  library('forecast')
  #####
  # Parameter inputs
  # Modify the items below to get different predictors returned
  #####
  stock<-'UPL'          # Enter Stock. Default UPL
  start_date<-20110101  # Enter beginning date. Format (YYYYMMDD)
  end_date <- 20140220   # Enter ending date. Format (YYYYMMDD)

  #########
  #  Predictors to be fetched
  # 
  #   Daily returns
  #   5 - day moving average
  #   20 - day moving average
  #   Ratio of 20 to 5 day MA
  #   5 day lagged % change in price
  #   20 day lagged % change in price
  #   Crude index - Symbol OIL = S&P GSCI Crude Oil TR Index ETN 
  #   Natural Gas Prices - Symbol UNG = US Nat'l Gas
  #   S&P 500 - Symbol SNP
  #
  #   Tomorrow's price <-- this is included for convenience as the predicted value
  #########
  
  #####
  # Retrieves stock data
  #####
  uplTS <- getYahooData(symbol=stock, start=start_date, end=end_date)
  uplTS.tomorrow <- lag(uplTS, k=-1)
  
  oilTS <- getYahooData(symbol="OIL", start=start_date, end=end_date)
  ngTS <- getYahooData(symbol="UNG", start=start_date, end=end_date)
  snpTS  <- getYahooData(symbol="SNP", start=start_date, end=end_date)
  
  #Generate daily returns for each of the TS retrieved
  upl.Ret <- ROC(uplTS[,"Close"])
  oil.Ret <- ROC(oilTS[,"Close"])
  ng.Ret <- ROC(ngTS[,"Close"])
  snp.Ret <- ROC(snpTS[,"Close"])
  
  #Generate log returns for each of the return series
  upl.log.Ret <- log(upl.Ret+1)
  oil.log.Ret <-log(oil.Ret+1)
  ng.log.Ret <- log(ng.Ret+1)
  snp.log.Ret <- log(snp.Ret+1)
  
  #Averages
  a.5day <- rollmean(uplTS[,"Close"], 5, align="right")
  a.20day <- rollmean(uplTS[,"Close"], 20, align="right")
  a.200day <- rollmean(uplTS[,"Close"], 200, align="right")
  
  #########
  #
  # Set up returned observations
  # To add more predictors, just add to the observation and names DF in the 
  #  same format as below
  #
  #########
  observationDF <- data.frame(coredata(a.5day["2013"]),
                        coredata(a.20day["2013"]),
                        coredata(a.200day["2013"]),
                        coredata(uplTS["2013","Close"]),
                        coredata(oilTS["2013","Close"]),
                        coredata(ngTS["2013","Close"]),
                        coredata(snpTS["2013","Close"]),
                        coredata(upl.Ret["2013"]),
                        coredata(oil.Ret["2013"]),
                        coredata(ng.Ret["2013"]),
                        coredata(snp.Ret["2013"]),
                        coredata(upl.log.Ret["2013"]),
                        coredata(oil.log.Ret["2013"]),
                        coredata(ng.log.Ret["2013"]),
                        coredata(snp.log.Ret["2013"]),
                        coredata(uplTS.tomorrow["2013","Close"])
                        )
  names(observationDF) <- c("avg5day",
                            "avg20day",
                            "avg200day", 
                            "upl.close",
                            "oil.close",
                            "ng.close",
                            "snp.close",
                            "upl.daily.return",
                            "oil.daily.return",
                            "ng.daily.return",
                            "snp.daily.return",
                            "upl.log.return",
                            "oil.log.return",
                            "ng.log.return",
                            "snp.log.return",
                            "upl.price.tomorrow"
                            )
  
  return(observationDF)
}