library(caret)
library(randomForest)
library(gbm)
library(plyr)
library(rpart) 
library(rpart.plot)
library(RColorBrewer)
library(rattle)

#load files
trainSRC <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
training <- read.csv(url(trainSRC), na.strings=c("NA","#DIV/0!",""))

blindSRC <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
blindSet <- read.csv(url(blindSRC), na.strings=c("NA","#DIV/0!",""))

#setup training and (testing)validation sets by spliting training data set into two
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)

trainingSet <- training[inTrain, ]
dim(trainingSet)

validateSet <- training[-inTrain, ]
dim(validateSet)

## Remove near zero variance columns
myDataNZV <- nearZeroVar(trainingSet, saveMetrics=TRUE)
myNZVvars2 <- rownames(myDataNZV[myDataNZV$nzv==FALSE,])
trainingSet <- subset(trainingSet, select=c(myNZVvars2))
trainingSet <- trainingSet[c(-1)]


## Remove columns with 80% or more 'na' data 
dummyDF <- 1 #create a dummy variable here to make append easier
for(i in 1:length(trainingSet)) { 
  if( sum( !is.na( trainingSet[i] ) ) /nrow(trainingSet) >= .8 ) {  
    if(is.data.frame(dummyDF))
    {dummyDF <- data.frame(dummyDF,trainingSet[i])}
    else
    {dummyDF <- data.frame(trainingSet[i])}
  }
}

summary(dummyDF)
trainingSet <- dummyDF
rm(dummyDF)

##also apply the right set of columns to training and incomoplete
usefulCols <- colnames(trainingSet)
validateSet <- validateSet[usefulCols]
usefulCols2 <- colnames(trainingSet[, -58]) #incomplete set is one classe column fewer
blindSet <- blindSet[usefulCols2]

##column count should match between training and validate. Incomplete set is missing classe
dim(trainingSet)
dim(validateSet)
dim(blindSet)

##force match data column class
for (i in 1:length(blindSet) ) {
  for(j in 1:length(trainingSet)) {
    if( length( grep(names(trainingSet[i]), names(blindSet)[j]) ) ==1)  {
      class(blindSet[j]) <- class(trainingSet[i])
    }      
  }      
}

blindSet <- rbind(trainingSet[2, -58] , blindSet)
blindSet <- blindSet[-1,]

####Decision Tree Model
modelRP <- rpart(classe ~ ., data=trainingSet, method="class")
predictRP <- predict(modelRP, validateSet, type = "class")
confusionMatrix(predictRP, validateSet$classe)

####Random Forest Model
modelRF <- randomForest(classe ~. , data=trainingSet)
predictRF <- predict(modelRF, validateSet, type = "class")
confusionMatrix(predictRF, validateSet$classe)

####Gradient Boosting Model
#modelGBM <- train(classe ~ ., method="gbm", data=trainingSet)
#predictGBM <- predict(modelGBM, validateSet, type = "class")
#confusionMatrix(predictGBM, validateSet$classe)

prediction.blind <- predict(modelRF, blindSet, type = "class")
prediction.blind

