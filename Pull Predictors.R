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
  
  
  #####
  # Retrieves stock data
  #####
  uplTS <- getYahooData(symbol=stock,
                        start=start_date,
                        end=end_date)

  lag1 <- lag(uplTS, k=1)

  #########
  # Initial predictors for playing around
  # 
  #   Daily returns
  #   5 - day moving average
  #   20 - day moving average
  #   Crude index 
  #
  #########
  
  d.R <- (uplTS[,"Close"] - lag1[,"Close"]) / lag1[,"Close"]
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
                        coredata(d.R["2013"])
                        )
  names(observationDF) <- c("avg5day",
                            "avg20day",
                            "avg200day", 
                            "close", 
                            "daily.return")
  
  return(observationDF)
}