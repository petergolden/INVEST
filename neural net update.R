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
  #First col of results is predicted, second is actual
{
  #RMSE Calc
  error <- results[,1] - results[,2]
  error <- error^2
  error <- sum(error)
  RMSE  <- sqrt(error)
  print(RMSE)
  
  #R Squared Calc
  SST <- sum((results[,2] - results[,1])^2)
  SSR <- sum((results[,2] - ave(results[,2]))^2)
  Rsquared <- 1-(SSR/SST)
  print(Rsquared)
  
  return(c(RMSE,Rsquared))
}

#########
#
# Train neural net - try to see what a good number of hidden nodes is to use
#
# Takes prohibitively long with 7 hidden nodes, no apparent gain in accuracy
#########
for (i in 1:20){
  set.seed(123)
  print(i)
  start.time <- proc.time()
  nn <- neuralnet( form, trainDF , hidden=i, threshold=0.01,stepmax=10^5)
  #nn <- neuralnet(ROR ~ avg5day + avg20day + avg200day, trainDF , hidden=i, threshold=0.01,stepmax=10^6)
  x<-proc.time() - start.time
  print(x)

  #Next lines needed to ensure nnet does not get cofused and start multiplying columns it shouldn't
  cov <- subset(trainDF, select = predCols)
  results<- compute(nn,cov)  
  
  allResults <- cbind(results$net.result, trainDF[response])
  calcStatistics(allResults)
 }
#Comment

#####
# Neural Net Running Time Frame
#
#########
rolling.result <- data.frame(numeric(),numeric())
actuals <- subset(fullDF, select = "upl.price.tomorrow")

for(i in 1:125){
  set.seed(123)
  print(paste("Calculating day", i))
  rollingTrain <- fullDF[i:(i+125),]
   
  nn <- neuralnet(form, rollingTrain c, hidden=2, threshold=0.01,stepmax=10^6)
  #nn <- neuralnet(upl.daily.return.tomorrow ~ a.5day + a.20day + a.200day, trainDF , hidden=i, threshold=0.01,stepmax=10^6)
  
  cov <- subset(fullDF[i+126,], select = predCols)
  result <- compute(nn,cov)
  rolling.result <- rbind(rolling.result,c(result$net.result,actuals[i+126,]))
}

calcStatistics(rolling.result)
