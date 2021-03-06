#INVEST TEAM - Transfer Function Code
#PREDICT 412

setwd("C:/Users/a19602/Documents/INVEST")

#initial installs addon package
install.packages('quantmod')
install.packages('forecast')
install.packages('TSA')
insall.packages('zoo')

library('forecast')
library('quantmod')
library('TSA')
library('zoo')
library('Metrics')

stock <- read.csv('UPL2013_fixed.csv',
                  colClasses = c("character","numeric","numeric","numeric"))

#convert factor to date
stock$DATE <- as.Date(stock$DATE, format = "%m/%d/%Y")

#subset the first half of the year to create the models
stock.firsthalf.2013 <- stock[1:182,]

#Graphs current data
pdf(file = "First_Half_2013_plot.pdf", width = 11, height = 8.5)
plot(stock.firsthalf.2013$CLOSE)
plot(stock.firsthalf.2013$WTI)
plot(stock.firsthalf.2013$NYMEX_NGAS)
dev.off()

#Log Transformation
#stock price may or maynot need to be log transformed. Each model will be built both ways
stock.firsthalf.2013$log_close<-log(stock.firsthalf.2013$CLOSE)
stock.firsthalf.2013$log_nymex_ngas<-log(stock.firsthalf.2013$NYMEX_NGAS)
stock.firsthalf.2013$log_WTI <- log(stock.firsthalf.2013$WTI)

#plot Log transformations
pdf(file = "logs_plot.pdf", width = 11, height = 8.5)
plot(stock.firsthalf.2013$log_close)
plot(stock.firsthalf.2013$log_nymex_ngas)
plot(stock.firsthalf.2013$log_WTI)
dev.off()

#NGas indicator appears to move in a similar pattern as UPL, 
#but the WTI does not appear to be related

#difference the transfer functions
log_nymex_ngas.diff <- diff(stock.firsthalf.2013$log_nymex_ngas, differences=1)
log_WTI.diff <- diff(stock.firsthalf.2013$log_WTI, differences=1)
log_close.diff <- diff(stock.firsthalf.2013$log_close, differences=1)

#plot the differenced ts
pdf(file = "difference_plot.pdf", width = 11, height = 8.5)
plot(log_nymex_ngas.diff)
plot(log_WTI.diff)
plot(log_close.diff)
dev.off()
#differenced log plots appear ready for model scoring

#Arima model - to score the stock price and the transfer functions
arima_model <- auto.arima(as.ts(log_close.diff))
arima_model.ngas <- auto.arima(as.ts(log_nymex_ngas.diff))
arima_model.WTI <- auto.arima(as.ts(log_WTI.diff))

#Summary of auto.arima models
summary(arima_model)
summary(arima_model.ngas)
summary(arima_model.WTI)

##ARIMAX CODE##
#nymex
arimax.upl.ngas <- arimax(log_close.diff, order=c(0,0,1), 
                          xtransf = log_nymex_ngas.diff, 
                          transfer=list(c(1,0)), method = 'ML')

#WTI
arimax.upl.WTI <- arimax(log_close.diff, order=c(0,0,1),
                         xtransf = log_WTI.diff, 
                         transfer=list(c(0,0)), method = 'ML') 

#summary of transfer function models
summary(arimax.upl.ngas)
summary(arimax.upl.WTI)

#Predict the two models
pred.ngas <- predict(arimax.upl.ngas , n.ahead=10)
pred.WTI <- predict(arimax.upl.WTI , n.ahead=10)

#plot predictions
plot(pred.ngas$se)
plot(pred.WTI$se)


#####ARIMAX with no log transformation to the stock price#######
#this code will look almost identitical to the above code only that the log transformation is removed
#from the stock

#remove the log transformation of the stock
close.diff <- exp(log_close.diff)

#nl for no log
#Arima model - to score the stock price and the transfer functions
arima_model.nl <- auto.arima(as.ts(close.diff))
arima_model.ngas <- auto.arima(as.ts(log_nymex_ngas.diff))
arima_model.WTI <- auto.arima(as.ts(log_WTI.diff))

#Summary of auto.arima models
summary(arima_model.nl)
summary(arima_model.ngas)
summary(arima_model.WTI)

##ARIMAX CODE - NO LOG##
#nymex
arimax.upl.ngas.nl <- arimax(close.diff, order=c(0,0,1), 
                             xtransf = log_nymex_ngas.diff, 
                             transfer=list(c(2,0)), method = 'ML')

#WTI
arimax.upl.WTI.nl <- arimax(close.diff, order=c(0,0,1),
                            xtransf = log_WTI.diff, 
                            transfer=list(c(0,0)), method = 'ML') 

#summary of transfer function models
summary(arimax.upl.ngas.nl)
summary(arimax.upl.WTI.nl)

#Predict the two models
pred.ngas.nl <- predict(arimax.upl.ngas.nl , n.ahead=10)
pred.WTI.nl <- predict(arimax.upl.WTI.nl , n.ahead=10)

#plot predictions
plot(pred.ngas.nl$se)
plot(pred.WTI.nl$se)

###Prepare data frames for rolling window###

#Log Transformation to entire 2013 data frame
stock$log_close<-log(stock$CLOSE)
stock$log_nymex_ngas<-log(stock$NYMEX_NGAS)
stock$log_WTI <- log(stock$WTI)

#difference the transfer functions
log_nymex_ngas.diff.fullyear <- diff(stock$log_nymex_ngas, differences=1)
log_WTI.diff.fullyear <- diff(stock$log_WTI, differences=1)
log_close.diff.fullyear <- diff(stock$log_close, differences=1)

##create full 2013 data fame for rolling window
teststock <- data.frame(log_close.diff = c(log_close.diff.fullyear),
                        log_nymex_ngas.diff = c(log_nymex_ngas.diff.fullyear),
                        log_WTI.diff = c(log_WTI.diff.fullyear))

##create full 2013 data fame for rolling window with no log transformation on the stock
teststock.nl <- data.frame(close.diff = c(exp(log_close.diff.fullyear)),
                           log_nymex_ngas.diff = c(log_nymex_ngas.diff.fullyear),
                           log_WTI.diff = c(log_WTI.diff.fullyear))

#initialize empty results DF
roll.results.nymex.log <- data.frame (numeric(),numeric())
roll.results.wti.log <- data.frame (numeric(),numeric())
roll.results.nymex.nl <- data.frame (numeric(),numeric())
roll.results.wti.nl <- data.frame (numeric(),numeric())

for (i in 1:181){
  
  roll.nymex.log <- arimax(teststock$log_close.diff[i:(i+182)], order=c(0,0,1), 
                           xtransf = teststock$log_nymex_ngas.diff[i:(i+182)], 
                           transfer=list(c(2,0)), method = 'ML')
  
  roll.wti.log <- arimax(teststock$log_close.diff[i:(i+182)], order=c(0,0,1),
                         xtransf = teststock$log_WTI.diff[i:(i+182)], 
                         transfer=list(c(0,0)), method = 'ML')  
  
  roll.nymex.nl <- arimax(teststock.nl$close.diff[i:(i+182)], order=c(0,0,1), 
                          xtransf = teststock.nl$log_nymex_ngas.diff[i:(i+182)], 
                          transfer=list(c(2,0)), method = 'ML')
  
  roll.wti.nl <- arimax(teststock.nl$close.diff[i:(i+182)], order=c(0,0,1),
                        xtransf = teststock.nl$log_WTI.diff[i:(i+182)], 
                        transfer=list(c(0,0)), method = 'ML') 
  
  
  tmp.a <- predict(roll.nymex.log, n.ahead=1)[2]$se
  tmp.b <- predict(roll.wti.log, n.ahead=1)[2]$se
  tmp.c <- predict(roll.nymex.nl, n.ahead=1)[2]$se
  tmp.d <- predict(roll.wti.nl, n.ahead=1)[2]$se
  
  roll.results.nymex.log <- rbind(roll.results.nymex.log, c(tmp.a,teststock$log_close.diff[i+182]))
  roll.results.wti.log <- rbind(roll.results.wti.log, c(tmp.b,teststock$log_close.diff[i+182]))
  roll.results.nymex.nl <- rbind(roll.results.nymex.nl, c(tmp.c, teststock.nl$close.diff[i+182]))
  roll.results.wti.nl <- rbind(roll.results.wti.nl, c(tmp.d, teststock.nl$close.diff[i+182]))
}

colnames(roll.results.nymex.log) <- c('Predict','Actual')
colnames(roll.results.nymex.nl) <- c('Predict','Actual')
colnames(roll.results.wti.log) <- c('Predict','Actual')
colnames(roll.results.wti.nl) <- c('Predict','Actual')


Rsquared <- function(x,y){
  r <- cor(x,y)
  Rsquared <- r^2
  return(Rsquared)
}

Rsquared(roll.results.wti.log[,1],roll.results.wti.log[,2])
Rsquared(roll.results.wti.nl[,1],roll.results.wti.nl[,2])
Rsquared(roll.results.nymex.log[,1],roll.results.nymex.log[,2])
Rsquared(roll.results.nymex.nl[,1],roll.results.wti.nl[,2])

rmse(roll.results.wti.log[,1],roll.results.wti.log[,2])
rmse(roll.results.wti.nl[,1],roll.results.wti.nl[,2])
rmse(roll.results.nymex.log[,1],roll.results.nymex.log[,2])
rmse(roll.results.nymex.nl[,1],roll.results.wti.nl[,2])
