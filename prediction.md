---
title: 'Peer-graded Assignment: Prediction Assignment Writeup'
author: "Jash Mehta"
date: '2 july 2020'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


#Objective

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

#Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

#Load the data

```{r, results=FALSE}
library(caret)
library(Hmisc)
library(corrplot)
library(e1071)
```

First we download our data and we read them in R.

```{r}
set.seed(2343)
train = read.csv("E:/r/machine_learning-/pml-training.csv")
quiz = read.csv("E:/r/machine_learning-/pml-testing.csv")
```

We take a look at data to see what kind of cleaning we need to do.

```{r}
set.seed(2343)
summary(train)
str(train)
```

#Cleaning

With the help of the function summary and str we could find that the data have have the following problems: a. presence of some characters such as "#DIV/0!". b. Many variables have a lot of NA values. c. We hane quite a few near-zero-variance variables.

```{r}
set.seed(2343)
train = read.csv("pml-training.csv", na.strings=c("#DIV/0!"))
train.nZV <- nearZeroVar(train)
train<-train[ , -train.nZV]
variables <- colnames(train[colSums(is.na(train)) == 0])
data1<- train[variables]
working.data <- data1[ , -(1:6)]
dim(working.data)
```

#Data partition and correlation

We seperate our original daaset into a training set and a testing set for cross validation. We choose 70\% of our original data to be the training set and 30\% the testing set.

Then we check what is the correlation between our variables. 

```{r}
set.seed(2343)
inTrain <- createDataPartition(y=working.data$classe, p=0.7, list=FALSE )
training <- working.data[inTrain,]
testing <- working.data[-inTrain,]
M <- cor(training[ , -53])
corrplot(M, order = "FPC", method = "square")
```

Variables that are highly correlated will appear with darker colour (red or blue). Most of the input variables have weak correlation (if any). Therefore we will proceed with the modeling but, we will have a few different prediction models.


#Model selection

We use three different methods and then we will calculate that treir confussion matrix in order to find their accuracy.


a. Generalized Boosted Model:

```{r}
set.seed(2343)
TCgbm <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
fit2  <- train(classe ~ ., data = training, method = "gbm",
                  trControl = TCgbm, verbose = FALSE)
predict2<- predict(fit2, newdata = testing)
confusionMatrix(predict2, testing$classe)
```

b. Random Forest:

```{r}
set.seed(2343)
TCrf <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
fit3  <- train(classe ~ ., data = training, method = "rf",
                  trControl = TCrf, verbose = FALSE)
predict3 <- predict(fit3, newdata = testing)
confusionMatrix(predict3, testing$classe)
```

Our first model the Support Vector Machine has accuracy of 95.4\%. Our second model the Generalized Boosted Model has accuracy of 96.1\%. Finally our last model has accuracy of 99.4\%. Now let us see what is the out-of-sample error for our last and most accurate model.


#Out-of-sample error

```{r}
set.seed(2343)
out.of.sample.error = function(values, predicted) {
  sum(predicted != values) / length(values)
}
out.of.sample.error(testing$classe, predict3)
```

The out-of-sample error for our last and most accurate model is 0.6\% as we would expect.

#Conclusion

In this project we used data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants in order to see how well they do the exercises. The fact that we found three models with accuracy above 95\% is quite impressive. This result indicates that the participates were really serious and dedicated to do the exercises properly.