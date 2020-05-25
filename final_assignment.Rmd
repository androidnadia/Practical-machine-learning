---
title: "Practical machine learning"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction / Summary
Electronic watches can collect a large amount of data about ones personal activities. 
These type of devices are taking measurements about physical activities to improve health, 
to find patterns in behavior, etc. People quantify regularly how much of a particular 
activity they do, but they rarely quantify how well they do it. In this project, the data 
from accelerometers on the belt, forearm, arm, and dumbell of 6 participants will be used. 
They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 
The datasets required for the present assignment is given by courtesy from [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har).   

## Purpose 
The goal is to predict the manner in which the participants did the exercise, which you 
find in the "classe" variable in the training set and predict it using the different parameters as predictors. Different models will be used and the one that suits best the data will be selected and used on the testing dataset. 

## Datasets
The training and testing datasets are downloaded and loaded to R environment. 
```{r}
fileurl_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileurl_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileurl_training, destfile = "pml-training.csv", mode = "wb")
download.file(fileurl_testing, destfile = "pml-testing.csv", mode = "wb")
training <- read.csv("pml-training.csv", header = TRUE, na.strings = c("", "NA"))
testing <- read.csv("pml-testing.csv", header = TRUE, na.strings = c("", "NA"))
```


## Exploraatory analysis
An overview of the dataset training is first performed. The dataset contains many columns that contains missing values. In order to reduce the analysis to essentials parameters, columns that contains more than 90% of missing values are removed, as well as the one that contains participants information.
```{r results="hide"}
library(dplyr, warn.conflicts = FALSE)
dim(training)
head(training)
missing_values <- vector()
for(i in 1:dim(training)[2]){missing_values[i] <- 100 * mean(is.na(training[,i]))}
training <- training[, which(missing_values < 90)] 
training <- training[, -c(1:7)]
training$classe <-  as.factor(training$classe)
str(training)
testing <- testing[, which(missing_values < 90)]
testing <- testing[, -c(1:7)]
```


## Predicition models
Since the training set is a large data frame, it will be subdivided into a training set and a cross validation test set prior using the model to the testing set. 
```{r}
library(caret)
library(e1071)
trainIndex = createDataPartition(y= training$classe,p=0.75,list=FALSE)
training_new = training[trainIndex,]
cross_valid = training[-trainIndex,]
```

Different classification algorithm are tested in parallel. The model selected are decision tree, random forest, bossting and a linear model. Each of the following subsection is dedicated to one model. The models are tested on the cross validation dataset created above.

### Classification tree
```{r}
set.seed(1234)

modFit0 <- train(classe ~., method ="rpart", data=training_new)
pred0 <- predict(modFit0, cross_valid)
confusionMatrix(pred0, cross_valid$classe)$overall["Accuracy"]
library(rattle, warn.conflicts = FALSE)
fancyRpartPlot(modFit0$finalModel)
```

### Random forest
```{r}
library(randomForest, warn.conflicts = FALSE)
modFit1 <- randomForest(classe ~., data = training_new, importance = FALSE)
pred1 <- predict(modFit1, cross_valid)
confusionMatrix(pred1, cross_valid$classe)$overall["Accuracy"]
```

### Boosting
```{r cache=TRUE}
modFit2 <- train(classe ~., method ="gbm", data=training_new, verbose = FALSE)
pred2 <- predict(modFit2, cross_valid)
confusionMatrix(pred2, cross_valid$classe)$overall["Accuracy"]
```

### Linear discriminant analysis
```{r}
modFit3 <- train(classe ~., method ="lda", data=training_new)
pred3 <- predict(modFit3, cross_valid)
confusionMatrix(pred3, cross_valid$classe)$overall["Accuracy"]
```

### Results and conclusion
First, a preprocessing of the dataset doen't add anything to the models (tested but not shown). The decision tree model doesn't perform well compared to the other models; the accuracy is only 50% and some classes are not represented. The linear discriminant accuracy is better than the decision tree, but is low compared to random forest and bossting models. The boositng model accuracy is high (about 97%), however it requires too much computation time.  The model that works best for this specific dataset is random forest; the accuracy is 99%. Hence, this model will be used on the testing dataset.

## RandomForest model of testing dataset
The random forest model is applied to the testing dataset and the results of the prediction is printed. The decreasing importance of the variables is shown below.
```{r}
predTest <- predict(modFit1, testing)
predTest
names(testing[,order(importance(modFit1), decreasing = TRUE)])
```




