library(rpart)
library(readr)
library(ggplot2)
#data
iris_data <- iris
wine <- read_csv("OneDrive - University of Reading/PHD DATA MINING/Coursework/wine.csv")
breast <- read_csv("OneDrive - University of Reading/PHD DATA MINING/Coursework/breast.csv")
bank <- read_csv("OneDrive - University of Reading/PHD DATA MINING/Coursework/data_banknote_authentication.csv")

####Resub + Hold Out Method IRIS

#Number of rows in iris data set
myData <- iris_data
N <- nrow(myData)
#Number of trials
numTrials <- 20
resub_accuracy_iris <- c()
hold_out_accuracy_iris <- c()

for(i in 1:numTrials){
  #Random sample (10% for the test set)
  randSample <- sample(N,(N*0.1)) 
  
  #Splitting Iris into train and test
  trainingSample <- myData[-randSample,]
  testSample <- myData[randSample,]
  
  
  #creating the tree
  dt <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainingSample, method = "class")
  
  #Applying model to training sample
  predictTraining <- predict(dt, trainingSample, type = 'class')
  
  #Testing accuracy
  comparePredictTraining <- table(trainingSample$Species,predictTraining)
  resub_accuracy_1 <- sum(diag(comparePredictTraining)) / sum(comparePredictTraining)
  resub_accuracy_iris <- append(resub_accuracy_iris,resub_accuracy_1)
  
  #Applying model to test sample
  predictTest <- predict(dt, testSample, type = 'class')
  
  #Testing accuracy
  comparePredictTest <- table(testSample$Species,predictTest)
  hold_out_accuracy_1 <- sum(diag(comparePredictTest)) / sum(comparePredictTest)
  hold_out_accuracy_iris <- append(hold_out_accuracy_iris,hold_out_accuracy_1)
}

#compute average and std of resub_accuracy
averageResub_iris <- mean(resub_accuracy_iris)
sdResub_iris <- sd(resub_accuracy_iris)

#compute average and std of hold_out_accuracy
averageHoldOut_iris <- mean(hold_out_accuracy_iris)
sdHoldOut_iris <- sd(hold_out_accuracy_iris)

###10-FoldCV IRIS
head(myData)

#create 10 partitions of input data
folds <- cut(seq(1,nrow(myData)),breaks=10,labels=FALSE)

xvalAccuracy_iris <- c()
for(i in 1:numTrials){
  #shuffle input data
  shuffle <- sample(nrow(myData))
  shuffledData <- myData[shuffle,]
  correctPrediction <- c()
  
  #Perform 10 fold cross validation
  for(j in 1:10){
    
    #Splitting Iris into train and test
    #if folds == J then that is the test Data
    #sample which folds == j
    testIDx <- ((j-1)*(N/10)):(((N/10)*j)-1)
    length(testIDx)
    
    trainData <- shuffledData[-testIDx,]
    testData <- shuffledData[testIDx,]
    
    #build model
    dt1 <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainData, method = "class")
    
    predictTest1 <- predict(dt1, testData, type = 'class')
    
    comparePredictTest1 <- table(testData$Species,predictTest1)
    correctPrediction1 <- sum(diag(comparePredictTest1))
    correctPrediction1
    
    correctPrediction <- append(correctPrediction, correctPrediction1)
    sumCorrectPrediction <- sum(correctPrediction)
    correctPrediction
    
  }
  
  xvalAccuracy1 <- sumCorrectPrediction/N
  xvalAccuracy_iris <- append(xvalAccuracy_iris, xvalAccuracy1)
  xvalAccuracy_iris
}

#mean and standard deviation of xvalaccuracy
averageXVal_iris <- mean(xvalAccuracy_iris)
sdXVal_iris <- sd(xvalAccuracy_iris)


####TASK 3 - LOOCV IRIS

sampleSize <- N*0.9 + 1
loocv_accuracy_iris <- c()

for(i in 1:numTrials){
  
  #sampling the data
  data <- sample(N, sampleSize)
  data <- myData[data, ]
  correct_prediction <- c()
  
  for(i in 1:sampleSize){
    #leaving one out
    testIndexes <- i
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    
    
    #build model
    dt2 <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = trainData, method = "class")
    
    #applying model to test data
    correct_prediction1 <- predict(dt2, testData, type = 'class')
    
    
    #Testing accuracy
    comparePredictTest2 <- table(testData$Species,correct_prediction1)
    if(testData$Species == correct_prediction1){
      correct_prediction_2<- sum(comparePredictTest2)
      correct_prediction <- append(correct_prediction, correct_prediction_2)}
    
  }
  
  loocv_accuracy1 <- sum(correct_prediction)/N
  loocv_accuracy_iris <- append(loocv_accuracy_iris,loocv_accuracy1)
}

averageLOOCV_iris <- mean(loocv_accuracy_iris)
sdLOOCV_iris <- sd(loocv_accuracy_iris)



####Resub + Hold Out Method WINE

#Number of rows in iris data set
myData <- wine
N <- nrow(myData)
#Number of trials
numTrials <- 20
resub_accuracy_wine <- c()
hold_out_accuracy_wine <- c()

for(i in 1:numTrials){
  #Random sample (10% for the test set)
  randSample <- sample(N,(N*0.1)) 
  
  #Splitting Iris into train and test
  trainingSample <- myData[-randSample,]
  testSample <- myData[randSample,]
  
  
  #creating the tree
  dt <- rpart(Class ~ Alcohol + Malic.Acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity + Hue + OD280.OD315.of.diluted.wines + Proline, data = trainingSample, method = "class")
  
  #Applying model to training sample
  predictTraining <- predict(dt, trainingSample, type = 'class')
  
  #Testing accuracy
  comparePredictTraining <- table(trainingSample$Class,predictTraining)
  resub_accuracy_1 <- sum(diag(comparePredictTraining)) / sum(comparePredictTraining)
  resub_accuracy_wine <- append(resub_accuracy_wine,resub_accuracy_1)
  
  #Applying model to test sample
  predictTest <- predict(dt, testSample, type = 'class')
  
  #Testing accuracy
  comparePredictTest <- table(testSample$Class,predictTest)
  hold_out_accuracy_1 <- sum(diag(comparePredictTest)) / sum(comparePredictTest)
  hold_out_accuracy_wine <- append(hold_out_accuracy_wine,hold_out_accuracy_1)
}

#compute average and std of resub_accuracy
averageResub_wine <- mean(resub_accuracy_wine)
sdResub_wine <- sd(resub_accuracy_wine)

#compute average and std of hold_out_accuracy
averageHoldOut_wine <- mean(hold_out_accuracy_wine)
sdHoldOut_wine <- sd(hold_out_accuracy_wine)

###10-FoldCV WINE
head(myData)

#create 10 partitions of input data
folds <- cut(seq(1,nrow(myData)),breaks=10,labels=FALSE)


xvalAccuracy_wine <- c()
for(i in 1:numTrials){
  #shuffle input data
  shuffle <- sample(nrow(myData))
  shuffledData <- myData[shuffle,]
  correctPrediction <- c()
  
  
  #Perform 10 fold cross validation
  for(j in 1:10){
    
    #Splitting Iris into train and test
    #if folds == J then that is the test Data
    #sample which folds == j
    testIDx <- ((j-1)*(N/10)):(((N/10)*j)-1)
    length(testIDx)
    
    trainData <- shuffledData[-testIDx,]
    testData <- shuffledData[testIDx,]
    
    #build model
    dt1 <- rpart(Class ~ Alcohol + Malic.Acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity + Hue + OD280.OD315.of.diluted.wines + Proline, data = trainData, method = "class")
    
    predictTest <- predict(dt1, testData, type = 'class')
    
    
    comparePredictTest1 <- table(testData$Class,predictTest)
    correctPrediction1 <- sum(diag(comparePredictTest1))
    correctPrediction1
    
    correctPrediction <- append(correctPrediction, correctPrediction1)
    sumCorrectPrediction <- sum(correctPrediction)
    correctPrediction
    
  }
  
  xvalAccuracy1 <- sumCorrectPrediction/N
  xvalAccuracy_wine <- append(xvalAccuracy_wine, xvalAccuracy1)
}

#mean and standard deviation of xvalaccuracy
averageXVal_wine <- mean(xvalAccuracy_wine)
sdXVal_wine <- sd(xvalAccuracy_wine)



####TASK 3 - LOOCV WINE

sampleSize <- N*0.9 + 1
loocv_accuracy_wine <- c()

for(i in 1:numTrials){
  
  #sampling the data
  data <- sample(N, sampleSize)
  data <- myData[data, ]
  correct_prediction <- c()
  
  for(i in 1:sampleSize){
    #leaving one out
    testIndexes <- i
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    
    
    #build model
    dt2 <- rpart(Class ~ Alcohol + Malic.Acid + Ash + Alcalinity.of.ash + Magnesium + Total.phenols + Flavanoids + Nonflavanoid.phenols + Proanthocyanins + Color.intensity + Hue + OD280.OD315.of.diluted.wines + Proline, data = trainData, method = "class")
    
    #applying model to test data
    correct_prediction1 <- predict(dt2, testData, type = 'class')
    
    #Testing accuracy
    comparePredictTest2 <- table(testData$Class,correct_prediction1)
    if(testData$Class == correct_prediction1){
      correct_prediction_2<- sum(comparePredictTest2)
      correct_prediction <- append(correct_prediction, correct_prediction_2)}
    sum_correct_prediction <- sum(correct_prediction)
    
  }
  
  loocv_accuracy1 <- sum(correct_prediction)/N
  loocv_accuracy_wine <- append(loocv_accuracy_wine,loocv_accuracy1)
}

averageLOOCV_wine <- mean(loocv_accuracy_wine)
sdLOOCV_wine <-sd(loocv_accuracy_wine)

####Resub + Hold Out Method BANK

#Number of rows in iris data set
myData <- bank
N <- nrow(myData)
#Number of trials
numTrials <- 20
resub_accuracy_bank <- c()
hold_out_accuracy_bank <- c()

for(i in 1:numTrials){
  #Random sample (10% for the test set)
  randSample <- sample(N,(N*0.1)) 
  
  #Splitting Iris into train and test
  trainingSample <- myData[-randSample,]
  testSample <- myData[randSample,]
  
  
  #creating the tree
  dt <- rpart(Class ~ Variance.Wavelet.Transformed.image + Skewness.Wavelet.Transformed.image + Kurtosis.Wavelet.Transformed.image + Entropy.image, data = trainingSample, method = "class")
  
  #Applying model to training sample
  predictTraining <- predict(dt, trainingSample, type = 'class')
  
  #Testing accuracy
  comparePredictTraining <- table(trainingSample$Class,predictTraining)
  resub_accuracy_1 <- sum(diag(comparePredictTraining)) / sum(comparePredictTraining)
  resub_accuracy_bank <- append(resub_accuracy_bank,resub_accuracy_1)
  
  #Applying model to test sample
  predictTest <- predict(dt, testSample, type = 'class')
  
  #Testing accuracy
  comparePredictTest <- table(testSample$Class,predictTest)
  hold_out_accuracy_1 <- sum(diag(comparePredictTest)) / sum(comparePredictTest)
  hold_out_accuracy_bank <- append(hold_out_accuracy_bank,hold_out_accuracy_1)
}

#compute average and std of resub_accuracy
averageResub_bank <- mean(resub_accuracy_bank)
sdResub_bank <- sd(resub_accuracy_bank)

#compute average and std of hold_out_accuracy
averageHoldOut_bank <- mean(hold_out_accuracy_bank)
sdHoldOut_bank <- sd(hold_out_accuracy_bank)

##10-FoldCV BANK
head(myData)

#create 10 partitions of input data
folds <- cut(seq(1,nrow(myData)),breaks=10,labels=FALSE)


xvalAccuracy_bank <- c()
for(i in 1:numTrials){
  #shuffle input data
  shuffle <- sample(nrow(myData))
  shuffledData <- myData[shuffle,]
  correctPrediction <- c()
  
  
  #Perform 10 fold cross validation
  for(j in 1:10){
    
    #Splitting Iris into train and test
    #if folds == J then that is the test Data
    #sample which folds == j
    testIDx <- ((j-1)*(N/10)):(((N/10)*j)-1)
    length(testIDx)
    
    trainData <- shuffledData[-testIDx,]
    testData <- shuffledData[testIDx,]
    
    #build model
    dt1 <- rpart(Class ~  Variance.Wavelet.Transformed.image + Skewness.Wavelet.Transformed.image + Kurtosis.Wavelet.Transformed.image + Entropy.image, data = trainData, method = "class")
    
    predictTest <- predict(dt1, testData, type = 'class')
    
    
    comparePredictTest1 <- table(testData$Class,predictTest)
    correctPrediction1 <- sum(diag(comparePredictTest1))
    correctPrediction1
    
    correctPrediction <- append(correctPrediction, correctPrediction1)
    sumCorrectPrediction <- sum(correctPrediction)
    correctPrediction
    
  }
  
  xvalAccuracy1 <- sumCorrectPrediction/N
  xvalAccuracy_bank <- append(xvalAccuracy_bank, xvalAccuracy1)
}

#mean and standard deviation of xvalaccuracy
averageXVal_bank <- mean(xvalAccuracy_bank)
sdXVal_bank<- sd(xvalAccuracy_bank)



####TASK 3 - LOOCV BANK

sampleSize <- N*0.9 + 1
loocv_accuracy_bank <- c()

for(i in 1:numTrials){
  
  #sampling the data
  data <- sample(N, sampleSize)
  data <- myData[data, ]
  correct_prediction <- c()
  
  for(i in 1:sampleSize){
    #leaving one out
    testIndexes <- i
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    
    
    #build model
    dt2 <- rpart(Class ~ Variance.Wavelet.Transformed.image + Skewness.Wavelet.Transformed.image + Kurtosis.Wavelet.Transformed.image + Entropy.image, data = trainData, method = "class")
    
    #applying model to test data
    correct_prediction1 <- predict(dt2, testData, type = 'class')
    
    
    #Testing accuracy
    comparePredictTest2 <- table(testData$Class,correct_prediction1)
    correct_prediction_2<- sum(diag(comparePredictTest2))
    correct_prediction <- append(correct_prediction, correct_prediction_2)
    sum_correct_prediction <- sum(correct_prediction)
    
  }
  
  loocv_accuracy1 <- sum_correct_prediction/N
  loocv_accuracy_bank <- append(loocv_accuracy_bank,loocv_accuracy1)
}

averageLOOCV_bank <- mean(loocv_accuracy_bank)
sdLOOCV_bank <-sd(loocv_accuracy_bank)


###Resub + Hold Out Method BREAST

#Number of rows in iris data set
myData <- breast
N <- nrow(myData)
#Number of trials
numTrials <- 20
resub_accuracy_breast <- c()
hold_out_accuracy_breast <- c()

for(i in 1:numTrials){
  #Random sample (10% for the test set)
  randSample <- sample(N,(N*0.1)) 
  
  #Splitting Iris into train and test
  trainingSample <- myData[-randSample,]
  testSample <- myData[randSample,]
  
  
  #creating the tree
  dt <- rpart(Class.Two.Benign.Four.Malignant ~ Clump.Thickness + Uniformity.Cell.Size + Uniformity.of.Cell.Shape + Marginal.Adhesion + Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses, data = trainingSample, method = "class")
  
  #Applying model to training sample
  predictTraining <- predict(dt, trainingSample, type = 'class')
  
  #Testing accuracy
  comparePredictTraining <- table(trainingSample$Class.Two.Benign.Four.Malignant,predictTraining)
  resub_accuracy_1 <- sum(diag(comparePredictTraining)) / sum(comparePredictTraining)
  resub_accuracy_breast <- append(resub_accuracy_breast,resub_accuracy_1)
  
  #Applying model to test sample
  predictTest <- predict(dt, testSample, type = 'class')
  
  #Testing accuracy
  comparePredictTest <- table(testSample$Class.Two.Benign.Four.Malignant,predictTest)
  hold_out_accuracy_1 <- sum(diag(comparePredictTest)) / sum(comparePredictTest)
  hold_out_accuracy_breast <- append(hold_out_accuracy_breast,hold_out_accuracy_1)
}

#compute average and std of resub_accuracy
averageResub_breast <- mean(resub_accuracy_breast)
sdResub_breast <- sd(resub_accuracy_breast)

#compute average and std of hold_out_accuracy
averageHoldOut_breast <- mean(hold_out_accuracy_breast)
sdHoldOut_breast <- sd(hold_out_accuracy_breast)

###10-FoldCV BREAST
head(myData)

#create 10 partitions of input data
folds <- cut(seq(1,nrow(myData)),breaks=10,labels=FALSE)


xvalAccuracy_breast<- c()

for(i in 1:numTrials){
  #shuffle input data
  shuffle <- sample(nrow(myData))
  shuffledData <- myData[shuffle,]
  correctPrediction <- c()
  
  
  #Perform 10 fold cross validation
  for(j in 1:10){
    
    #Splitting Iris into train and test
    #if folds == J then that is the test Data
    #sample which folds == j
    testIDx <- ((j-1)*(N/10)):(((N/10)*j)-1)
    length(testIDx)
    
    trainData <- shuffledData[-testIDx,]
    testData <- shuffledData[testIDx,]
    
    #build model
    dt1 <- rpart(Class.Two.Benign.Four.Malignant ~ Clump.Thickness + Uniformity.Cell.Size + Uniformity.of.Cell.Shape + Marginal.Adhesion + Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses, data = trainData, method = "class")
    
    predictTest1 <- predict(dt1, testData, type = 'class')
    
    
    comparePredictTest1 <- table(testData$Class.Two.Benign.Four.Malignant,predictTest1)
    correctPrediction1 <- sum(diag(comparePredictTest1))
    correctPrediction1
    
    correctPrediction <- append(correctPrediction, correctPrediction1)
    sumCorrectPrediction <- sum(correctPrediction)
    correctPrediction
    
  }
  
  xvalAccuracy1 <- sumCorrectPrediction/N
  xvalAccuracy_breast <- append(xvalAccuracy_breast, xvalAccuracy1)
  
}

#mean and standard deviation of xvalaccuracy
averageXVal_breast <- mean(xvalAccuracy_breast)
sdXVal_breast <- sd(xvalAccuracy_breast)


####TASK 3 - LOOCV BREAST

sampleSize <- N*0.9 + 1
loocv_accuracy_breast <- c()

for(i in 1:numTrials){
  
  #sampling the data
  data <- sample(N, sampleSize)
  data <- myData[data, ]
  correct_prediction <- c()
  
  for(i in 1:sampleSize){
    #leaving one out
    testIndexes <- i
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    
    
    #build model
    dt2 <- rpart(Class.Two.Benign.Four.Malignant ~ Clump.Thickness + Uniformity.Cell.Size + Uniformity.of.Cell.Shape + Marginal.Adhesion + Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses, data = trainData, method = "class")
    
    #applying model to test data
    correct_prediction1 <- predict(dt2, testData, type = 'class')
    
    
    #Testing accuracy
    comparePredictTest2 <- table(testData$Class.Two.Benign.Four.Malignant,correct_prediction1)
    if(testData$Class.Two.Benign.Four.Malignant == correct_prediction1){
      correct_prediction_2<- sum(comparePredictTest2)
      correct_prediction <- append(correct_prediction, correct_prediction_2)}
    sum_correct_prediction <- sum(correct_prediction)
    
  }
  
  loocv_accuracy1 <- sum_correct_prediction/N
  loocv_accuracy_breast <- append(loocv_accuracy_breast,loocv_accuracy1)
}

averageLOOCV_breast <- mean(loocv_accuracy_breast)
sdLOOCV_breast <- sd(loocv_accuracy_breast)


# Creating charts comparing validation methods
val_methods <- c("Resub","HoldOut", "XVal", "LOOCV")
# IRIS data
mean.iris <- c(averageResub_iris,
averageHoldOut_iris,
averageXVal_iris,
averageLOOCV_iris)

sd.iris <- c(sdResub_iris,
sdHoldOut_iris,
sdXVal_iris,
sdLOOCV_iris)

iris_summary <- data.frame(val_methods,mean.iris,sd.iris)

Type = rep(val_methods,1)
Mean_SD = c(rep("mean",4),rep("sd",4))
value = c(round(averageResub_iris,digits = 3),
          round(averageHoldOut_iris,digits = 3),
          round(averageXVal_iris,digits = 3),
          round(averageLOOCV_iris,digits = 3),
          round(sdResub_iris,digits = 3),
          round(sdHoldOut_iris,digits = 3),
          round(sdXVal_iris,digits = 3),
          round(sdLOOCV_iris,digits = 3))


df.i <- data.frame(Type,Mean_SD,value)
ggplot(df.i, aes(Mean_SD, value, fill = Type))+ 
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  labs(title="Plot comparing accuracy of methods on IRIS data")




# WINE data
mean.wine <- c(averageResub_wine,
averageHoldOut_wine,
averageXVal_wine,
averageLOOCV_wine)


sd.wine <- c(sdResub_wine,
sdHoldOut_wine,
sdXVal_wine,
sdLOOCV_wine)

wine_summary <- data.frame(val_methods,mean.wine,sd.wine)

Type = rep(val_methods,1)
Mean_SD = c(rep("mean",4),rep("sd",4))
value = c(round(averageResub_wine,digits = 3),
          round(averageHoldOut_wine,digits = 3),
          round(averageXVal_wine,digits = 3),
          round(averageLOOCV_wine,digits = 3),
          round(sdResub_wine,digits = 3),
          round(sdHoldOut_wine,digits = 3),
            round(sdXVal_wine,digits = 3),
          round(sdLOOCV_wine,digits = 3))

df.w <- data.frame(Type,Mean_SD,value)
ggplot(df.w, aes(Mean_SD, value, fill = Type))+ 
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  labs(title="Plot comparing accuracy of methods on WINE data")


# BANK data
mean.bank <- c(averageResub_bank,
averageHoldOut_bank,
averageXVal_bank,
averageLOOCV_bank)

sd.bank <- c(sdResub_bank,
sdHoldOut_bank,
sdXVal_bank,
sdLOOCV_bank)

bank_summary <- data.frame(val_methods,mean.bank,sd.bank)


Type = rep(val_methods,1)
Mean_SD = c(rep("mean",4),rep("sd",4))
value = c(round(averageResub_bank,digits = 3),
          round(averageHoldOut_bank,digits = 3),
          round(averageXVal_bank,digits = 3),
          round(averageLOOCV_bank,digits = 3),
          round(sdResub_bank,digits = 3),
          round(sdHoldOut_bank,digits = 3),
          round(sdXVal_bank,digits = 3),
          round(sdLOOCV_bank,digits = 3))

df.ba <- data.frame(Type,Mean_SD,value)
ggplot(df.ba, aes(Mean_SD, value, fill = Type))+ 
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  labs(title="Plot comparing accuracy of methods on BANK data")

# BREAST data
mean.breast <- c(averageResub_breast,
averageHoldOut_breast,
averageXVal_breast,
averageLOOCV_breast)

sd.breast <- c(sdResub_breast,
sdHoldOut_breast,
sdXVal_breast,
sdLOOCV_breast)


Type = rep(val_methods,1)
Mean_SD = c(rep("mean",4),rep("sd",4))
value = c(round(averageResub_breast,digits = 3),
          round(averageHoldOut_breast,digits = 3),
          round(averageXVal_breast,digits = 3),
          round(averageLOOCV_breast,digits = 3),
          round(sdResub_breast,digits = 3),
          round(sdHoldOut_breast,digits = 3),
          round(sdXVal_breast,digits = 3),
          round(sdLOOCV_breast,digits = 3))

df.br <- data.frame(Type,Mean_SD,value)
ggplot(df.br, aes(Mean_SD, value, fill = Type))+ 
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  labs(title="Plot comparing accuracy of methods on BREAST data")

breast_summary <- data.frame(val_methods,mean.breast,sd.breast)