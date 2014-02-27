#Auto Arima Function

#set working directory
setwd("C:/Users/Jim Braun/Documents/Predict 412/R/Working Directory")

library(forecast)

# parameter inputs
stock<-'UPL' # Enter Stock. Default UPL
start_date<-20121201 # Enter beginning date. Format (YYYYMMDD)
end_date<- 20140220 # Enter ending date. Format (YYYYMMDD)

# retrieves stock data
uplTS <- getYahooData(symbol=stock,
                      start=start_date,
                      end=end_date)


#####################
#Log Transformation #
#####################
log_close<-log(uplTS$Close)

######################
#Graphs current data #
######################

pdf(file = "upl_plot.pdf", width = 11, height = 8.5)		##/\open pdf /\##
plot(uplTS$Close)
dev.off()									##\/close pdf\/##

pdf(file = "log_upl_plot.pdf", width = 11, height = 8.5)	##/\open pdf /\##
plot(log_close)
dev.off()									##\/close pdf\/##

##############################################
# Set up training and test sets (Dataframes) #
##############################################
trainDF <- data.frame(coredata(a.5day["2013"]),coredata(a.20day["2013"]),coredata(uplTS["2013","Close"]))
names(trainDF) <- c("avg5day","avg20day","close")

testDF <- data.frame(coredata(a.5day["2014"]),coredata(a.20day["2014"]),coredata(uplTS["2014","Close"]))
names(testDF) <- c("avg5day","avg20day","close")


str(trainDF$close)
str(testDF$close)

###############################
# Exponential Smoothing - ETS #
###############################

# Note model is A,N,N which means additive, no trend, no seasonality
# reference: http://www.inside-r.org/packages/cran/forecast/docs/ets
# Thus the forecast is the mean - not real exciting
# a forecast equal to the mean yields forecasts with no variation
# cannot calculate correlation and R-square with forecasts with no variation 

sink("Exponential_Smoothing_Summaries.txt") 		##/\ sink in /\##
# Fit Exponential Smoothing Model and get summaries
etsfit <-ets(trainDF$close)
summary(etsfit)
accuracy(etsfit)

# create ets forecast
ets_forecast <- forecast(etsfit)
summary(ets_forecast)

# run ets model on test set and test accuracy of model in test data
ets_test <-ets(testDF$close, model=etsfit)
accuracy(ets_test)

# test accuracy on fitted forecast vs. test set
accuracy(forecast(etsfit,10), testDF$close[1:10])
# create two vectors to create R-square
# one is the forecast; other is the test set
# since the forecast is the last value, will not work
# need to examine structure of forecast to find forecase vector
# looks like $x is the input TS and forecast is the vector called $mean
str(ets_forecast)
test_ten <- testDF$close[1:10]
test_ten
ets_forecast_ten <- as.numeric(ets_forecast$mean)
ets_forecast_ten
compare_ten <- cbind(test_ten, ets_forecast_ten)
cor(compare_ten)
round(ets_r_sq <- cor(ets_forecast$mean, testDF$close[1:10])^2, digits=3)
sink()								##\/ sink out\/##

# create all plots
pdf(file = "ets_plots.pdf", width = 11, height = 8.5)	##/\open pdf /\##
plot(etsfit)
plot(residuals(etsfit))
plot(ets_forecast)
plot(ets_test)
plot(compare_ten)
dev.off()								##\/close pdf\/##



################################################
#Arima model - automatically select the best   #
################################################

sink("Auto_Arima_Summaries.txt") 				##/\ sink in /\##

# auto.arima model based on untransformed close price
# yields a 0,1,1 model.  
aa_model<-auto.arima(trainDF$close)
summary(aa_model)
plot(aa_model)

# Generate forecast
# Forecast is a flat last observation again!
aa_forecast <- forecast(aa_model)
aa_forecast
str(aa_forecast)

# test accuracy on fitted forecast vs. test set
accuracy(forecast(aa_forecast,10), testDF$close[1:10])
# create two vectors to create R-square
# one is the forecast; other is the test set
# since the forecast is the last value, will not work
# need to examine structure of forecast to find forecase vector
# looks like $x is the input TS and forecast is the vector called $mean
aa_test_ten <- testDF$close[1:10]
aa_test_ten
aa_forecast_ten <- as.numeric(aa_forecast$mean)
aa_forecast_ten
compare_aa_ten <- cbind(aa_test_ten, aa_forecast_ten)
cor(compare_aa_ten)
round(ets_r_sq <- cor(aa_forecast$mean, testDF$close[1:10])^2, digits=3)
sink()								##\/ sink out\/##


# print plots
pdf(file = "aa_plots.pdf", width = 11, height = 8.5)	##/\open pdf /\##
plot(aa_forecast$residuals)
plot(aa_forecast$x)
plot(compare_aa_ten)
dev.off()								##\/close pdf\/##


##############################
# Auto Arima on Log of Close #
##############################

# Get log
aa_log_close<-log(trainDF$close)

aa_log_model<-auto.arima(aa_log_close)
structure(aa_log_model)
plot(aa_log_model)
summary(aa_log_model)

# Generate Forecast
aa_log_forecast <- forecast(aa_log_model)
aa_log_forecast 
str(aa_log_forecast)

# test accuracy on fitted forecast vs. test set
accuracy(forecast(aa_log_forecast,10), aa_log_close[1:10])
# create two vectors to create R-square
# one is the forecast; other is the test set
# since the forecast is the last value, will not work
# need to examine structure of forecast to find forecase vector
# looks like $x is the input TS and forecast is the vector called $mean
aa_log_test_ten <- aa_log_close[1:10]
aa_log_test_ten
aa_log_forecast_ten <- as.numeric(aa_log_forecast$mean)
aa_log_forecast_ten
compare_aa_log_ten <- cbind(aa_log_test_ten, aa_log_forecast_ten)
cor(compare_aa_log_ten)
round(ets_r_sq <- cor(aa_log_forecast$mean, aa_log_close[1:10])^2, digits=3)


# Get Plots
pdf(file = "aa_log_plots.pdf", width = 11, height = 8.5)	##/\open pdf /\##
plot(aa_log_forecast)
plot(aa_log_forecast$residuals)
plot(compare_aa_log_ten)
dev.off()									##\/close pdf\/##




