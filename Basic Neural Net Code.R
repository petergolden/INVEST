#INVEST TEAM - Basic Neural Net Code
#PREDICT 412

library('forecast')
library('quantmod')
library('zoo')
library('neuralnet')
# parameter inputs
stock<-'UPL'          # Enter Stock. Default UPL
start_date<-20110101  # Enter beginning date. Format (YYYYMMDD)
end_date<- 20140220   # Enter ending date. Format (YYYYMMDD)

# retrieves stock data
uplTS <- getYahooData(symbol=stock,
                      start=start_date,
                      end=end_date)

#########
# Initial predictors for playing around
# 
#   5 - day moving average
#   20 - day moving average
#   Crude index 
#
#########

a.5day <- rollmean(uplTS[,"Close"], 5, align="right")
a.20day <- rollmean(uplTS[,"Close"], 20, align="right")
a.200day <- rollmean(uplTS[,"Close"], 200, align="right")
#########
#
# Set up training and test sets (Dataframes)
#
#########
trainDF <- data.frame(coredata(a.5day["2013"]),
                      coredata(a.20day["2013"]),
                      coredata(a.200day["2013"]),
                      coredata(uplTS["2013","Close"]))
names(trainDF) <- c("avg5day","avg20day", "avg200day", "close")

testDF <- data.frame(coredata(a.5day["2014"]),
                     coredata(a.20day["2014"]),
                     coredata(a.200day["2014"])
                     )
names(testDF) <- c("avg5day","avg20day", "avg200day") #,"close")


#########
#
# Train neural net
#
#########
nn <- neuralnet(close ~ avg5day + avg20day + avg200day, trainDF , hidden=10, threshold=0.01)

#Comment

#########
#
# Predict Results and evaluate model
#
#########
predictions <- compute(nn,testDF)

resultsMatrix <- matrix(c(coredata(uplTS["2014","Close"],predictions$net.result)), ncol=2)
rownames(resultsMatrix) <- c("Predicted","Actual")
print(resultsMatrix)
