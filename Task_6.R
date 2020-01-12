library(rpart)
library(readr)
library(ggplot2)
#data

breast <- read_csv("OneDrive - University of Reading/PHD DATA MINING/Coursework/breast.csv")
sample_size <- c(99,199,299,399,499,599)
resub_acc_ss <- c()
resub_sd_ss <- c()
holdout_acc_ss <- c()
holdout_sd_ss <- c()
xfold_acc_ss <- c()
xfold_sd_ss <- c()
LOOCV_acc_ss <- c()
LOOCV_sd_ss <- c()
###Resub + Hold Out Method BREAST

#Number of rows in iris data set
myData <- breast
N <- nrow(myData)
#Number of trials
numTrials <- 20

for(j in 1:length(sample_size)){
resub_accuracy_breast <- c()
hold_out_accuracy_breast <- c()

for(i in 1:numTrials){
  #Random sample (10% for the test set)
  randSample <- sample(N,sample_size[j]) 
  #Splitting Iris into train and test
  trainingSample <- myData[randSample,]
  testSample <- myData[-randSample,]
  
  
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
resub_acc_ss<- append(resub_acc_ss,averageResub_breast)
resub_sd_ss <- append(resub_sd_ss,sdResub_breast)
#compute average and std of hold_out_accuracy
averageHoldOut_breast <- mean(hold_out_accuracy_breast)
sdHoldOut_breast <- sd(hold_out_accuracy_breast)
holdout_acc_ss <- append(holdout_acc_ss,averageHoldOut_breast)
holdout_sd_ss <- append(holdout_sd_ss,sdHoldOut_breast)
}
###10-FoldCV BREAST
head(myData)

#create 10 partitions of input data
folds <- cut(seq(1,nrow(myData)),breaks=10,labels=FALSE)

for(k in 1:length(sample_size)){
  sample <- sample(N,sample_size[k]) 
  sampled <- myData[sample,]
  xvalAccuracy_breast<- c()

for(i in 1:numTrials){
  
  #shuffle input data
  shuffle <- sample(nrow(sampled))
  shuffledData <- sampled[shuffle,]
  
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
xfold_acc_ss <- append(xfold_acc_ss,averageXVal_breast)
xfold_sd_ss <- append(xfold_sd_ss,sdXVal_breast)
}

####TASK 3 - LOOCV BREAST
for(k in 1:length(sample_size)){
sampleSize <- sample_size[k] + 1
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
LOOCV_acc_ss <- append(LOOCV_acc_ss,averageLOOCV_breast)
LOOCV_sd_ss <- append(LOOCV_sd_ss, sdLOOCV_breast)
}


#RESUB

size_of_sample <- c(rep("099",2),rep("199",2),rep("299",2),rep("399",2),rep("499",2),rep("599",2))
Type = c(rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1))
value = c(round(resub_acc_ss[1],digits = 3),round(resub_sd_ss[1],digits = 3),
          round(resub_acc_ss[2],digits = 3),round(resub_sd_ss[2],digits = 3),
          round(resub_acc_ss[3],digits = 3),round(resub_sd_ss[3],digits = 3),
          round(resub_acc_ss[4],digits = 3),round(resub_sd_ss[4],digits = 3),
          round(resub_acc_ss[5],digits = 3),round(resub_sd_ss[5],digits = 3),
          round(resub_acc_ss[6],digits = 3),round(resub_sd_ss[6],digits = 3))

df.resub <- data.frame(Type,size_of_sample,value)
ggplot(df.resub, aes(size_of_sample, value, fill = Type))+ 
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  labs(title="Plot comparing accuracy of RESUB method across sample sized on Breast data")



#HOLDOUT
size_of_sample <- c(rep("099",2),rep("199",2),rep("299",2),rep("399",2),rep("499",2),rep("599",2))
Type = c(rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1))
value = c(round(holdout_acc_ss[1],digits = 3),round(holdout_sd_ss[1],digits = 3),
          round(holdout_acc_ss[2],digits = 3),round(holdout_sd_ss[2],digits = 3),
          round(holdout_acc_ss[3],digits = 3),round(holdout_sd_ss[3],digits = 3),
          round(holdout_acc_ss[4],digits = 3),round(holdout_sd_ss[4],digits = 3),
          round(holdout_acc_ss[5],digits = 3),round(holdout_sd_ss[5],digits = 3),
          round(holdout_acc_ss[6],digits = 3),round(holdout_sd_ss[6],digits = 3))

df.holdout <- data.frame(Type,size_of_sample,value)
ggplot(df.holdout, aes(size_of_sample, value, fill = Type))+ 
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  labs(title="Plot comparing accuracy of HOLDOUT method across sample sized on Breast data")


#XFOLD
size_of_sample <- c(rep("099",2),rep("199",2),rep("299",2),rep("399",2),rep("499",2),rep("599",2))
Type = c(rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1))
value = c(round(xfold_acc_ss[1],digits = 3),round(xfold_sd_ss[1],digits = 3),
          round(xfold_acc_ss[2],digits = 3),round(xfold_sd_ss[2],digits = 3),
          round(xfold_acc_ss[3],digits = 3),round(xfold_sd_ss[3],digits = 3),
          round(xfold_acc_ss[4],digits = 3),round(xfold_sd_ss[4],digits = 3),
          round(xfold_acc_ss[5],digits = 3),round(xfold_sd_ss[5],digits = 3),
          round(xfold_acc_ss[6],digits = 3),round(xfold_sd_ss[6],digits = 3))

df.xfold <- data.frame(Type,size_of_sample,value)
ggplot(df.xfold, aes(size_of_sample, value, fill = Type))+ 
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  labs(title="Plot comparing accuracy of XFOLD method across sample sized on Breast data")

#LOOCV
size_of_sample <- c(rep("099",2),rep("199",2),rep("299",2),rep("399",2),rep("499",2),rep("599",2))
Type = c(rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1),rep("mean",1),rep("sd",1))
value = c(round(LOOCV_acc_ss[1],digits = 3),round(LOOCV_sd_ss[1],digits = 3),
          round(LOOCV_acc_ss[2],digits = 3),round(LOOCV_sd_ss[2],digits = 3),
          round(LOOCV_acc_ss[3],digits = 3),round(LOOCV_sd_ss[3],digits = 3),
          round(LOOCV_acc_ss[4],digits = 3),round(LOOCV_sd_ss[4],digits = 3),
          round(LOOCV_acc_ss[5],digits = 3),round(LOOCV_sd_ss[5],digits = 3),
          round(LOOCV_acc_ss[6],digits = 3),round(LOOCV_sd_ss[6],digits = 3))

df.LOOCV <- data.frame(Type,size_of_sample,value)
ggplot(df.LOOCV, aes(size_of_sample, value, fill = Type))+ 
  geom_bar(stat="identity", position = position_dodge())+
  geom_text(aes(label=value), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3)+
  labs(title="Plot comparing accuracy of LOOCV method across sample sized on Breast data")




