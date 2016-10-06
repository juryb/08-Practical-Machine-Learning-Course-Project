library (caret)
library(rpart)
library(randomForest)
library(kernlab)


setwd("~/08-Practical-Machine-Learning-Course-Project")
dataTrainRAW <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
dataTestRAW <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

dim(dataTrain)
dim(dataTest)
summary(dataTrain)

sum(is.na(dataTrain))
colSums(is.na(dataTrain))

#clean data
# Remove columns full of NAs.
ccc <- colSums(is.na(dataTrainRAW))==0
dataTrain <-dataTrainRAW[,ccc]
dataTest <-dataTestRAW[,ccc]

# remove variables with nearly zero variance
nzv <- nearZeroVar(dataTrain, saveMetrics = TRUE)
dataTrain <- dataTrain[, -nzv$nzv]
dataTest <- dataTest[, -nzv$nzv]

#remove variables that don't make intuitive sense for prediction
dataTrain <-dataTrain[, -c(1:6)]
dataTest <-dataTest[, -c(1:6)]
#remove problem_id from Test set
dataTest <- dataTest[,1:52]

summary(dataTrain$classe)

# train and test set
#split the cleaned training set into a pure training data set (70%) 
#and a validation data set (30%). 
#We will use the validation data set to conduct cross validation in future steps
set.seed(12345)
inTrain <- createDataPartition(y=dataTrain$classe, p=0.7, list=FALSE)
training <- dataTrain[inTrain, ]
validation <- dataTrain[-inTrain, ]


##build models
#train the model by using all remaining variables in the data set. 

# instruct train to use 3-fold CV to select optimal tuning parameters
fitControl <- trainControl(method="cv", number=3, verboseIter=FALSE)

#CART - classification and regression trees
CART.fit <- train(classe ~ ., data=training, method="rpart", trControl=fitControl)

#Random Forest
RF.fit <- train(classe ~ ., method="rf", data=training, trControl=fitControl)

#LDA - Linear Discriminant Analysis Model
LDA.fit <- train(classe ~ ., data=training, method="lda", trControl=fitControl)

#kNN - k-nearest neighbors 
KNN.fit <- train(classe ~ ., data=training, method="knn", trControl=fitControl)

#SVM with RBF - support vector machine with radial basis function 
SVM.fit <- train(classe ~ ., data=training, method="svmRadial", trControl=fitControl)

#Gradient Boosting w/ Trees
#gbm.Fit <- train(classe ~ ., data=training, method="gbm", verbose=FALSE)

# collect resamples
results <- resamples(list(CART=CART.fit, LDA=LDA.fit, SVM=SVM.fit, KNN=KNN.fit, RF=RF.fit))
# summarize differences between models

summary(results)


#use the random forest method since it is one of the most accurate methods
RF.fit$finalModel


# use model to predict classe in validation set
predict1 <- predict(RF.fit, newdata=validation)

# show confusion matrix to get estimate of out-of-sample error
confusionMatrix(validation$classe, predict1)

#the estimated accuracy of the model is 98.85% and the estimated out-of-sample error is 1,15%.


#Variable Importance

#carve out 10% of the sub training data and use a random forest to determine variable importance.
#use this information to cull out any variables that are deemed unimportant

#inVarImp <- createDataPartition(y=dataTrain$classe, p=0.1, list=FALSE) 
finalTrainData <- dataTrain[-inVarImp, ]

varImpObj <- varImp(RF.fit)
plot(varImpObj, main = "Variable Importance of Top 25", top = 25)


impThresh <- quantile(varImpObj$importance[, 1], 0.75)
impfilter <- varImpObj$importance[, 1] >= impThresh

finalTrainData <- finalTrainData[, impfilter]

rfModel <- train(classe ~ ., data = finalTrainData, method = "rf")
rfModel$finalModel

# use model to predict classe in validation set
finalTestData <- dataTest[, impfilter]

predict2 <- predict(rfModel, newdata=validation)

# show confusion matrix to get estimate of out-of-sample error
confusionMatrix(validation$classe, predict2)

#the estimated accuracy of the model is 99.85% and the estimated out-of-sample error is 0,15%.


#Predicting for Test Data Set
#apply the model to the original testing data set downloaded from the data source


#predict in original Testing data
result <- predict(modelFit, newdata=dataTest)
result



