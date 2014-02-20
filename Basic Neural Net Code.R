#INVEST TEAM - Basic Neural Net Code
#PREDICT 412

library('forecast')
library('quantmod')

# parameter inputs
stock<-'UPL'          # Enter Stock. Default UPL
train.start_date<-20130101  # Enter beginning date. Format (YYYYMMDD)
train.end_date<- 20131231   # Enter ending date. Format (YYYYMMDD)
test.start_date <- 20140101
test.end_date <- 20140220

# retrieves stock data
trainData<-getYahooData(symbol=stock,
                   start=train.start_date,
                   end=train.end_date
)

testData<-getYahooData(symbol=stock,
                       start=test.start_date,
                       end=test.end_date
)
