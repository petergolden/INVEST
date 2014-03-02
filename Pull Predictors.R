#INVEST TEAM 
#PREDICT 412
#Code to pull predictors and place into a DF for use

loadPredictorObservations <- function(){
  #source("http://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/FinancialInstrument/inst/parser/download.tblox.R?root=blotter") 
  library('forecast')
  library('quantmod')
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
  #10 Year treasury note TS
  tnxTS <- getYahooData(symbol="TNX", start=start_date, end=end_date)
  #20 Year Treasury TS
  tltTS <- getYahooData(symbol="TLT", start=start_date, end=end_date)
  vxxTS <- getYahooData(symbol="VXX", start=start_date, end=end_date)
  uupTS <- getYahooData(symbol="UUP", start=start_date, end=end_date)
  fxeTS <- getYahooData(symbol="FXE", start=start_date, end=end_date)
  
  #need to use a different function to get exchange rates
  toss <- getFX("CAD/JPY", from="2012-01-01", to="2013-02-20")
  cadjpyTS <- CADJPY
  toss2 <- getFX("CAD/JPY", from="2013-02-21", to="2014-02-20")
  cadjpyTS <- rbind( cadjpyTS, CADJPY ) 
  
  #Generate daily returns for each of the TS retrieved
  upl.Ret <- ROC(uplTS[,"Close"])
  upl.Ret.tomorrow <- lag(upl.Ret, k=-1)
  oil.Ret <- ROC(oilTS[,"Close"])
  ng.Ret <- ROC(ngTS[,"Close"])
  snp.Ret <- ROC(snpTS[,"Close"])
  tnx.Ret <- ROC(tnxTS[,"Close"])
  tlt.Ret <- ROC(tltTS[,"Close"])
  vxx.Ret <- ROC(vxxTS[,"Close"])
  uup.Ret <- ROC(uupTS[,"Close"])
  fxe.Ret <- ROC(fxeTS[,"Close"])
  cadjpy.Ret <- ROC(cadjpyTS[,"CAD.JPY"])
  
  #Generate log returns for each of the return series
  upl.log.Ret <- log(upl.Ret+1)
  upl.log.Ret.tomorrow <- lag(upl.log.Ret, k=-1)
  oil.log.Ret <-log(oil.Ret+1)
  ng.log.Ret <- log(ng.Ret+1)
  snp.log.Ret <- log(snp.Ret+1)
  tnx.log.Ret <- log(tnx.Ret+1)
  tlt.log.Ret <- log(tlt.Ret+1)
  vxx.log.Ret <- log(vxx.Ret+1)
  uup.log.Ret <- log(uup.Ret+1)
  fxe.log.Ret <- log(fxe.Ret+1)
  cadjpy.log.Ret <- log(cadjpy.Ret+1)
  
  #Averages
  a.5day <- rollmean(uplTS[,"Close"], 5, align="right")
  a.20day <- rollmean(uplTS[,"Close"], 20, align="right")
  a.200day <- rollmean(uplTS[,"Close"], 200, align="right")
  a.oil.5day <- rollmean(oilTS[,"Close"], 5, align="right")
  a.oil.20day <- rollmean(oilTS[,"Close"], 20, align="right")
  a.oil.200day <- rollmean(oilTS[,"Close"], 200, align="right")
  a.ng.5day <- rollmean(ngTS[,"Close"], 5, align="right")
  a.ng.20day <- rollmean(ngTS[,"Close"], 20, align="right")
  a.ng.200day <- rollmean(ngTS[,"Close"], 200, align="right")
  a.snp.5day <- rollmean(snpTS[,"Close"], 5, align="right")
  a.snp.20day <- rollmean(snpTS[,"Close"], 20, align="right")
  a.snp.200day <- rollmean(snpTS[,"Close"], 200, align="right")
  a.tnx.5day <- rollmean(tnxTS[,"Close"], 5, align="right")
  a.tnx.20day <- rollmean(tnxTS[,"Close"], 20, align="right")
  a.tnx.200day <- rollmean(tnxTS[,"Close"], 200, align="right")
  a.tlt.5day <- rollmean(tltTS[,"Close"], 5, align="right")
  a.tlt.20day <- rollmean(tltTS[,"Close"], 20, align="right")
  a.tlt.200day <- rollmean(tltTS[,"Close"], 200, align="right")
  a.vxx.5day <- rollmean(vxxTS[,"Close"], 5, align="right")
  a.vxx.20day <- rollmean(vxxTS[,"Close"], 20, align="right")
  a.vxx.200day <- rollmean(vxxTS[,"Close"], 200, align="right")
  a.uup.5day <- rollmean(uupTS[,"Close"], 5, align="right")
  a.uup.20day <- rollmean(uupTS[,"Close"], 20, align="right")
  a.uup.200day <- rollmean(uupTS[,"Close"], 200, align="right")
  a.fxe.5day <- rollmean(fxeTS[,"Close"], 5, align="right")
  a.fxe.20day <- rollmean(fxeTS[,"Close"], 20, align="right")
  a.fxe.200day <- rollmean(fxeTS[,"Close"], 200, align="right")
  a.cadjpy.5day <- rollmean(cadjpyTS[,"CAD.JPY"], 5, align="right")
  a.cadjpy.20day <- rollmean(cadjpyTS[,"CAD.JPY"], 20, align="right")
  a.cadjpy.200day <- rollmean(cadjpyTS[,"CAD.JPY"], 200, align="right")
  
  a.log.Ret.5day <- rollmean(upl.log.Ret[-1,"Close"], 5, align="right")
  a.log.Ret.20day <- rollmean(upl.log.Ret[-1,"Close"], 20, align="right")
  a.log.Ret.200day <- rollmean(upl.log.Ret[-1,"Close"], 200, align="right")
  a.oil.log.Ret.5day <- rollmean(oil.log.Ret[-1,"Close"], 5, align="right")
  a.oil.log.Ret.20day <- rollmean(oil.log.Ret[-1,"Close"], 20, align="right")
  a.oil.log.Ret.200day <- rollmean(oil.log.Ret[-1,"Close"], 200, align="right")
  a.ng.log.Ret.5day <- rollmean(ng.log.Ret[-1,"Close"], 5, align="right")
  a.ng.log.Ret.20day <- rollmean(ng.log.Ret[-1,"Close"], 20, align="right")
  a.ng.log.Ret.200day <- rollmean(ng.log.Ret[-1,"Close"], 200, align="right")
  a.snp.log.Ret.5day <- rollmean(snp.log.Ret[-1,"Close"], 5, align="right")
  a.snp.log.Ret.20day <- rollmean(snp.log.Ret[-1,"Close"], 20, align="right")
  a.snp.log.Ret.200day <- rollmean(snp.log.Ret[-1,"Close"], 200, align="right")
  a.tnx.log.Ret.5day <- rollmean(tnx.log.Ret[-1,"Close"], 5, align="right")
  a.tnx.log.Ret.20day <- rollmean(tnx.log.Ret[-1,"Close"], 20, align="right")
  a.tnx.log.Ret.200day <- rollmean(tnx.log.Ret[-1,"Close"], 200, align="right")
  a.tlt.log.Ret.5day <- rollmean(tlt.log.Ret[-1,"Close"], 5, align="right")
  a.tlt.log.Ret.20day <- rollmean(tlt.log.Ret[-1,"Close"], 20, align="right")
  a.tlt.log.Ret.200day <- rollmean(tlt.log.Ret[-1,"Close"], 200, align="right")
  a.vxx.log.Ret.5day <- rollmean(vxx.log.Ret[-1,"Close"], 5, align="right")
  a.vxx.log.Ret.20day <- rollmean(vxx.log.Ret[-1,"Close"], 20, align="right")
  a.vxx.log.Ret.200day <- rollmean(vxx.log.Ret[-1,"Close"], 200, align="right")
  a.uup.log.Ret.5day <- rollmean(uup.log.Ret[-1,"Close"], 5, align="right")
  a.uup.log.Ret.20day <- rollmean(uup.log.Ret[-1,"Close"], 20, align="right")
  a.uup.log.Ret.200day <- rollmean(uup.log.Ret[-1,"Close"], 200, align="right")
  a.fxe.log.Ret.5day <- rollmean(fxe.log.Ret[-1,"Close"], 5, align="right")
  a.fxe.log.Ret.20day <- rollmean(fxe.log.Ret[-1,"Close"], 20, align="right")
  a.fxe.log.Ret.200day <- rollmean(fxe.log.Ret[-1,"Close"], 200, align="right")
  a.cadjpy.log.Ret.5day <- rollmean(cadjpy.log.Ret[-1,"CAD.JPY"], 5, align="right")
  a.cadjpy.log.Ret.20day <- rollmean(cadjpy.log.Ret[-1,"CAD.JPY"], 20, align="right")
  a.cadjpy.log.Ret.200day <- rollmean(cadjpy.log.Ret[-1,"CAD.JPY"], 200, align="right")
  
  
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
                        coredata(uplTS["2013","Close"]) ,
                        coredata(oilTS["2013","Close"]) ,
                        coredata(ngTS["2013","Close"]) ,
                        coredata(snpTS["2013","Close"]) , #
                        coredata(upl.Ret["2013"]),
                        coredata(oil.Ret["2013"]),
                        coredata(ng.Ret["2013"]),
                        coredata(snp.Ret["2013"]),
                        coredata(tnx.Ret["2013"]),
                        coredata(tlt.Ret["2013"]),
                        coredata(vxx.Ret["2013"]),
                        coredata(uup.Ret["2013"]),
                        coredata(fxe.Ret["2013"]),
                        coredata(cadjpy.Ret[as.Date(index(a.fxe.200day["2013",]))]),
                        coredata(upl.log.Ret["2013"]),
                        coredata(oil.log.Ret["2013"]),
                        coredata(ng.log.Ret["2013"]),
                        coredata(snp.log.Ret["2013"]),
                        coredata(tnx.log.Ret["2013"]),
                        coredata(tlt.log.Ret["2013"]),
                        coredata(vxx.log.Ret["2013"]),
                        coredata(uup.log.Ret["2013"]),
                        coredata(fxe.log.Ret["2013"]),
                        coredata(cadjpy.log.Ret[as.Date(index(a.fxe.200day["2013",]))]),
                        coredata(a.oil.5day["2013"]),
                        coredata(a.oil.20day["2013"]),
                        coredata(a.oil.200day["2013"]),
                        coredata(a.ng.5day["2013"]),
                        coredata(a.ng.20day["2013"]) , #
                        coredata(a.ng.200day["2013"]),
                        coredata(a.snp.5day["2013"]),
                        coredata(a.snp.20day["2013"]),
                        coredata(a.snp.200day["2013"]),
                        coredata(a.tnx.5day["2013"]),
                        coredata(a.tnx.20day["2013"]),
                        coredata(a.tnx.200day["2013"]),
                        coredata(a.tlt.5day["2013"]),
                        coredata(a.tlt.20day["2013"]),
                        coredata(a.tlt.200day["2013"]),
                        coredata(a.vxx.5day["2013"]),
                        coredata(a.vxx.20day["2013"]),
                        coredata(a.vxx.200day["2013"]) , 
                        coredata(a.uup.5day["2013"]),
                        coredata(a.uup.20day["2013"]),
                        coredata(a.uup.200day["2013"]),
                        coredata(a.fxe.5day["2013"]),
                        coredata(a.fxe.20day["2013"]),
                        coredata(a.fxe.200day["2013"]) ,
                        #FX has different trading dates, so easiest is just use
                        #the indices of the stock days to grab the needed 
                        # FX indices
                        coredata(a.cadjpy.5day[as.Date(index(a.fxe.200day["2013",]))]),
                        coredata(a.cadjpy.20day[as.Date(index(a.fxe.200day["2013",]))]),
                        coredata(a.cadjpy.200day[as.Date(index(a.fxe.200day["2013",]))]) ,
                        coredata(uplTS.tomorrow["2013","Close"]),
                        coredata(a.log.Ret.5day["2013"]),
                        coredata(a.log.Ret.20day["2013"]),
                        coredata(a.log.Ret.200day["2013"]),
                        coredata(a.oil.log.Ret.5day["2013"]),
                        coredata(a.oil.log.Ret.20day["2013"]),
                        coredata(a.oil.log.Ret.200day["2013"]),
                        coredata(a.ng.log.Ret.5day["2013"]),
                        coredata(a.ng.log.Ret.20day["2013"]) , #
                        coredata(a.ng.log.Ret.200day["2013"]),
                        coredata(a.snp.log.Ret.5day["2013"]),
                        coredata(a.snp.log.Ret.20day["2013"]),
                        coredata(a.snp.log.Ret.200day["2013"]),
                        coredata(a.tnx.log.Ret.5day["2013"]),
                        coredata(a.tnx.log.Ret.20day["2013"]),
                        coredata(a.tnx.log.Ret.200day["2013"]),
                        coredata(a.tlt.log.Ret.5day["2013"]),
                        coredata(a.tlt.log.Ret.20day["2013"]),
                        coredata(a.tlt.log.Ret.200day["2013"]),
                        coredata(a.vxx.log.Ret.5day["2013"]),
                        coredata(a.vxx.log.Ret.20day["2013"]),
                        coredata(a.vxx.log.Ret.200day["2013"]) , 
                        coredata(a.uup.log.Ret.5day["2013"]),
                        coredata(a.uup.log.Ret.20day["2013"]),
                        coredata(a.uup.log.Ret.200day["2013"]),
                        coredata(a.fxe.log.Ret.5day["2013"]),
                        coredata(a.fxe.log.Ret.20day["2013"]),
                        coredata(a.fxe.log.Ret.200day["2013"]),
                        coredata(a.cadjpy.log.Ret.5day[as.Date(index(a.fxe.200day["2013",]))]),
                        coredata(a.cadjpy.log.Ret.20day[as.Date(index(a.fxe.200day["2013",]))]),
                        coredata(a.cadjpy.log.Ret.200day[as.Date(index(a.fxe.200day["2013",]))]) ,
                        coredata(upl.Ret.tomorrow["2013","Close"]),
                        coredata(upl.log.Ret.tomorrow["2013","Close"])
                        )
  names(observationDF) <- c("a.5day",
                            "a.20day",
                            "a.200day", 
                            "upl.close",
                            "oil.close",
                            "ng.close",
                            "snp.close",
                            "upl.daily.return",
                            "oil.daily.return",
                            "ng.daily.return",
                            "snp.daily.return",
                            "tnx.daily.return",
                            "tlt.daily.return",
                            "vxx.daily.return",
                            "uup.daily.return",
                            "fxe.daily.return",
                            "cadjpy.daily.return",
                            "upl.log.return",
                            "oil.log.return",
                            "ng.log.return",
                            "snp.log.return",
                            "tnx.log.return",
                            "tlt.log.return",
                            "vxx.log.return",
                            "uup.log.return",
                            "fxe.log.return",
                            "cadjpy.log.return",
                            "a.oil.5day",
                            "a.oil.20day",
                            "a.oil.200day",
                            "a.ng.5day",
                            "a.ng.20day",
                            "a.ng.200day",
                            "a.snp.5day",
                            "a.snp.20day",
                            "a.snp.200day",
                            "a.tnx.5day",
                            "a.tnx.20day",
                            "a.tnx.200day",
                            "a.tlt.5day",
                            "a.tlt.20day",
                            "a.tlt.200day",
                            "a.vix.5day",
                            "a.vix.20day",
                            "a.vix.200day",
                            "a.uup.5day",
                            "a.uup.20day",
                            "a.uup.200day",
                            "a.fxe.5day",
                            "a.fxe.20day",
                            "a.fxe.200day",
                            "a.cadjpy.5day",
                            "a.cadjpy.20day",
                            "a.cadjpy.200day",
                            "upl.price.tomorrow",
                            "a.log.Ret.5day",
                            "a.log.Ret.20day",
                            "a.log.Ret.200day",
                            "a.oil.log.Ret.5day",
                            "a.oil.log.Ret.20day",
                            "a.oil.log.Ret.200day",
                            "a.ng.log.Ret.5day",
                            "a.ng.log.Ret.20day", #
                            "a.ng.log.Ret.200day",
                            "a.snp.log.Ret.5day",
                            "a.snp.log.Ret.20day",
                            "a.snp.log.Ret.200day",
                            "a.tnx.log.Ret.5day",
                            "a.tnx.log.Ret.20day",
                            "a.tnx.log.Ret.200day",
                            "a.tlt.log.Ret.5day",
                            "a.tlt.log.Ret.20day",
                            "a.tlt.log.Ret.200day",
                            "a.vxx.log.Ret.5day",
                            "a.vxx.log.Ret.20day",
                            "a.vxx.log.Ret.200day", 
                            "a.uup.log.Ret.5day",
                            "a.uup.log.Ret.20day",
                            "a.uup.log.Ret.200day",
                            "a.fxe.log.Ret.5day",
                            "a.fxe.log.Ret.20day",
                            "a.fxe.log.Ret.200day",
                            "a.cadjpy.log.Ret.5day",
                            "a.cadjpy.log.Ret.20day",
                            "a.cadjpy.log.Ret.200day",
                            "upl.daily.return.tomorrow",
                            "upl.log.return.tomorrow" 
                            )
  return(observationDF)
}

#Set up some useful scatterplots
printAvgCorrs <- function(symb)
{
  pdf(paste("correlations ", symb, ".pdf", sep=""))
  form <- formula(paste("~upl.price.tomorrow+a.",symb,".5day+a.",symb,".20day+a.",symb,".200day"  ,sep ="" ))
  pairs(form,observationDF)
  dev.off()
}

printRetCorrs <- function(symb)
{
  pdf(paste("return correlations -", symb, ".pdf", sep=""))
  form <- formula(paste("~upl.daily.return.tomorrow+",symb,".daily.return",sep ="" ))
  pairs(form,observationDF)
  dev.off()
  
  pdf(paste("log return correlations -", symb, ".pdf", sep=""))
  form <- formula(paste("~upl.log.return.tomorrow+", symb, ".log.return+a.",symb,".log.Ret.5day+a.",symb,".log.Ret.20day+a.",symb,".log.Ret.200day"  ,sep ="" ))
  pairs(form,observationDF)
  dev.off()
}

symbols <- as.matrix(c("cadjpy", "fxe", "uup", "vxx", "tlt", "tnx", "snp", "ng", "oil"))
apply(symbols,1,printAvgCorrs)
apply(symbols,1,printRetCorrs)


pairs(~upl.daily.return.tomorrow+fxe.daily.return+fxe.log.return,observationDF)

