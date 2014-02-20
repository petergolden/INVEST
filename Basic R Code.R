
#initial installs addon package
install.packages('quantmod')
install.packages('forecast')


library('forecast')
library('quantmod')

# parameter inputs
stock<-'UPL'          # Enter Stock. Default UPL
start_date<-20130101  # Enter beginning date. Format (YYYYMMDD)
end_date<- 20131231   # Enter ending date. Format (YYYYMMDD)

# retrieves stock data
data<-getYahooData(symbol=stock,
                   start=start_date,
                   end=end_date
)

#Graphs current data
plot(data$Close)

#Log Transformation
log_close<-log(data$Close)

#Arima model - automatically select the best
arima_model<-auto.arima(as.ts(log_close))

#Summary Staistics of the mode
summary(arima_model)

#plots the log of close plus the next 10 forecasted periods based on the fitted ARIMA model
plot(forecast(arima_model, h=10))

#gives the values of forcasted model
forecast(arima_model, h=10)