#INVEST TEAM - Basic Neural Net Code
#PREDICT 412

library('forecast')
library('quantmod')
library('zoo')
library('neuralnet')

source('./Pull Predictors.R')


##Initialize Data and predictors to be used

fullDF <- loadPredictorObservations()
trainDF <- fullDF[1:151,]
testDF <- fullDF[152:252,]

response <- "upl.price.tomorrow"
predictors <- "a.5day + a.20day + a.200day + cadjpy.close + vxx.close"
form <- formula(paste(response,"~",predictors,sep=""))
predCols <- unlist(strsplit(predictors," + ", fixed = TRUE))

#Calculates RMSE and RSquared
calcStatistics <- function( results )
{
  #RMSE Calc
  error <- results$net.result - trainDF[response]
  error <- error^2
  error <- sum(error)
  RMSE  <- sqrt(error)
  print(RMSE)
  
  #R Squared Calc
  SSR <- sum((trainDF[response] - results$net.result)^2)
  SST <- sum((trainDF[response] - ave(unlist(trainDF[response])))^2)
  Rsquared <- 1-(SSR/SST)
  print(Rsquared)
  
  return c(RMSE,Rsquared)
}

#########
#
# Train neural net - try to see what a good number of hidden nodes is to use
#
#########
for (i in 1:20){
  set.seed(123)
  print(i)
  start.time <- proc.time()
  nn <- neuralnet( form, trainDF , hidden=i, threshold=0.01,stepmax=10^6)
  #nn <- neuralnet(ROR ~ avg5day + avg20day + avg200day, trainDF , hidden=i, threshold=0.01,stepmax=10^6)
  x<-proc.time() - start.time
  print(x)

  #Next lines needed to ensure nnet does not get cofused and start multiplying columns it shouldn't
  cov <- subset(trainDF, select = predCols)
  results<- compute(nn,cov)  
  
  #RMSE Calc
  error <- results$net.result - trainDF[response]
  error <- error^2
  error <- sum(error)
  RMSE  <- sqrt(error)
  print(RMSE)
  
  #R Squared Calc
  SSR <- sum((trainDF[response] - results$net.result)^2)
  SST <- sum((trainDF[response] - ave(unlist(trainDF[response])))^2)
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
actuals <- subset(fullDF, select = "upl.price.tomorrow")

for(i in 1:125){
  set.seed(123)
  print(paste("Calculating day", i))
  rollingTrain <- fullDF[i:(i+125),]
   
  nn <- neuralnet(upl.price.tomorrow ~ a.5day + a.20day + a.200day, trainDF , hidden=3, threshold=0.01,stepmax=10^6)
  #nn <- neuralnet(upl.daily.return.tomorrow ~ a.5day + a.20day + a.200day, trainDF , hidden=i, threshold=0.01,stepmax=10^6)
  cols <- c("a.5day","a.20day","a.200day")
  cov <- subset(rollingTest, select = cols)
  result <- compute(nn,cov)
  rolling.result <- rbind(rolling.result,c(result$net.result,actuals[i+126,]))
}

#RMSE Calc
error <- rolling.result[,1] - rolling.result[,2]
error <- error^2
error <- sum(error)
RMSE  <- sqrt(error)
print(RMSE)

#R Squared Calc
SST <- sum((rolling.result[,2] - rolling.result[,1])^2)
SSR <- sum((rolling.result[,2] - ave(rolling.result[,2]))^2)
Rsquared <- 1-(SSR/SST)
print(Rsquared)

#########
#
# Predict Results and evaluate model
#
#########
#predictions <- compute(nn,testDF)

#resultsMatrix <- matrix(c(coredata(uplTS["2014","Close"],predictions$net.result)), ncol=2)
#rownames(resultsMatrix) <- c("Predicted","Actual")
#print(resultsMatrix)