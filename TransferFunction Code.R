#INVEST TEAM - Transfer Function Code
#PREDICT 412

#initial installs addon package
install.packages('quantmod')
install.packages('forecast')
install.packages('TSA')

library('forecast')
library('quantmod')
library('TSA')

#read it in file
stock <- read.csv('UPL.csv')

#convert factor to date
stock$DATE <- as.Date(stock$DATE, format = "%m/%d/%Y")

#Graphs current data
plot(stock$CLOSE)

#Log Transformation
stock$log_close<-log(stock$CLOSE)
stock$log_nymex_ngas<-log(stock$NYMEX_NGAS)
stock$log_WTI <- log(stock$WTI)

#plot Log transformations
plot(stock$log_close)
plot(stock$log_nymex_ngas)
plot(stock$log_WTI)

#difference the transfer functions
log_nymex_ngas.diff <- diff(stock$log_nymex_ngas, differences=1)
log_WTI.diff <- diff(stock$log_WTI, differences=1)
log_close.diff <- diff(stock$log_close, differences=1)

#Arima model - to score the stock price and the transfer functions
arima_model <- auto.arima(as.ts(log_close.diff))
arima_model.ngas <- auto.arima(as.ts(log_nymex_ngas.diff))
arima_model.WTI <- auto.arima(as.ts(log_WTI.diff))

##ARIMAX CODE##
#nymex
arimax.upl.nymex <- arimax(log_close.diff, order=c(1,0,1), 
                           xtransf = log_nymex_ngas.diff, 
                           transfer=list(c(3,3)), method = 'ML')

#WTI
arimax.upl.WTI <- arimax(log_close.diff, order=c(1,0,1),
                         xtransf = log_WTI.diff, 
                         transfer=list(c(0,0)), method = 'ML') 


