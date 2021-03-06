---
title: "Prediction Assignment Writeup"
output: 
  html_document:
    keep_md: true
---

```{r setup, include=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.path='Figs/')
```


## Executive Summary
The goal of this project is to predict the manner in which they did the exercise with the data given in the training data set. After reading and clearning data, the training data set was seperated into training and validation dataset. Then 3 different algorithms were used for modelling and the results were analyzed in terms of performance. Best models were selected, based on maximum accuracy, and used to predict the training set. 

### set seed and load packages
```{r load-packages, message=FALSE}
set.seed(07012016)
library(caret)
library(plyr)
```

### data cleaning
Read training and testing dataset. Since some variables like time has nothing to do with the prediction, those columns were deleted from dataset
```{r load-data, cache=TRUE, message=FALSE}

trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainset <- read.csv(url(trainUrl), na.strings = c("NA", "#DIV/0!", ""))
dim(trainset)
trainset <- trainset[,-c(1:7)]

testset <- read.csv(url(testUrl), na.strings = c("NA", "#DIV/0!", ""))
dim(testset)
testset <- testset[,-c(1:7)]


```

Cleaning dataset was performed by removing columns with more than half NA. In addition, training dataset was split into training and validation dataset (6:4 ratio).
```{r cleaning-data, message=FALSE}
CountNA <- sapply(colnames(trainset), function(x) ifelse(sum(is.na(trainset[,x])) > 0.5*nrow(trainset), FALSE, TRUE ))
trainset <- trainset[, CountNA]
dim(trainset)

inTrain <- createDataPartition(y=trainset$classe, p=0.6, list = FALSE)
trainsetTrain <- trainset[inTrain,]
trainsetTest <- trainset[-inTrain,]
dim(trainsetTrain)
dim(trainsetTest)
```

3 different modeling techniques were used here, including decision tree, random forest, and boosting. Cross validation was used (k fold with K=4) and pca was used for demension reduction. The performance was calculated and shown with confusionMatrix function. It is apparent that random forest has highest accuracy (~0.99) and decision tree is worse (~0.5 accuracy).
```{r modeling, cache=TRUE, message=FALSE}
trainMC <- trainControl(method = "cv", number = 4, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)

trainTree <- train(classe ~ ., data = trainsetTrain, method = "rpart", trControl= trainMC)
ptrainTree <- predict(trainTree, trainsetTest) 
confusionMatrix(ptrainTree,trainsetTest$classe)

trainRf <- train(classe ~ ., data = trainsetTrain, method = "rf", trControl= trainMC)
ptrainRf <- predict(trainRf, trainsetTest) 
confusionMatrix(ptrainRf,trainsetTest$classe)


trainBoosting <- train(classe ~ ., data = trainsetTrain, method = "gbm", trControl= trainMC)
ptrainBoosting <- predict(trainBoosting, trainsetTest)
confusionMatrix(ptrainBoosting,trainsetTest$classe)

```

Random forest was used here for final prediction on test dataset since it has highest accuracy, verified with validation dataset.
```{r testset-prediction, cache=TRUE, message=FALSE}
pfinaltest <- predict(trainRf, testset)
pfinaltest

```
