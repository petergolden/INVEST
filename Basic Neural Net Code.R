#INVEST TEAM - Basic Neural Net Code
#PREDICT 412

library('forecast')
library('quantmod')
library('zoo')
library('neuralnet')

set.seed(2014)
# parameter inputs
stock<-'UPL' # Enter Stock. Default UPL
start_date<-20110101 # Enter beginning date. Format (YYYYMMDD)
end_date<- 20140220 # Enter ending date. Format (YYYYMMDD)

# retrieves stock data
uplTS <- getYahooData(symbol=stock,
                      start=start_date,
                      end=end_date)

#########
# Initial predictors for playing around
#
# 5 - day moving average
# 20 - day moving average
# Crude index
#
#########
a.1dayROR <- ROC(uplTS[,'Close'])
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
                      coredata(uplTS["2013","Close"]),
                      coredata(a.1dayROR["2013"]))
names(trainDF) <- c("avg5day","avg20day", "avg200day", "close","ROR")

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
for (i in 1:20){
  set.seed(123)
  print(i)
  start.time <- proc.time()
  nn <- neuralnet(ROR ~ avg5day + avg20day + avg200day, trainDF , hidden=i, threshold=0.01,stepmax=10^6)
  x<-proc.time() - start.time
  print(x)
  
  results<- compute(nn,testDF)  
  
  #RMSE Calc
  error <- results$net.result - coredata(a.1dayROR["2014"])
  error <- error^2
  error <- sum(error)
  RMSE  <- sqrt(error)
  print(RMSE)
  
  #R Squared Calc
  SST <- sum((coredata(a.1dayROR["2014"]) - ave(coredata(a.1dayROR["2014"])))^2)
  SSR <- sum((results$net.result - ave(coredata(a.1dayROR["2014"])))^2)
  Rsquared <- 1-(SSR/SST)
  print(Rsquared)
}
#Comment


#########
#
# Neural Net Running Time Frame
#
#########
rolling.result <- data.frame(numeric(),numeric())
for(i in 1:125){
  rollingTrain <- trainDF[i:(i+125),]
  rollingTest <- trainDF[i+126,1:3]
  set.seed(123)
  nn <- neuralnet(ROR ~ avg5day + avg20day + avg200day, rollingTrain , hidden=9, threshold=0.01,
                  stepmax=10^6)
  result <- compute(nn,rollingTest)
  rolling.result <- rbind(rolling.result,c(result$net.result,trainDF[i+126,'ROR']))
}

#RMSE Calc
error <- rolling.result[,1] - rolling.result[,2]
error <- error^2
error <- sum(error)
RMSE  <- sqrt(error)
print(RMSE)

#R Squared Calc
SST <- sum(rolling.result[,2] - ave(rolling.result[,2])^2)
SSR <- sum(rolling.result[,1] - ave(rolling.result[,2])^2)
Rsquared <- 1-(SSR/SST)
print(Rsquared)

#########
#
# Predict Results and evaluate model
#
#########
predictions <- compute(nn,testDF)

resultsMatrix <- matrix(c(coredata(uplTS["2014","Close"],predictions$net.result)), ncol=2)
rownames(resultsMatrix) <- c("Predicted","Actual")
print(resultsMatrix)