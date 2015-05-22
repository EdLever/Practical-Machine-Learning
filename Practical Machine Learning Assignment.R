

library(rattle)
library(dplyr)
library(caret)
library(randomForest)


setwd("C:\\Users\\Eduardo\\Desktop\\Coursera")
rawtesting <- read.csv("pml-testing.csv", na.strings = c("","NA", "NULL"))
rawtraining <- read.csv("pml-training.csv", na.strings= c("","NA", "NULL"))


#Preprocessing the Data

dim(rawtraining)

As you can notice this data set is particularly large, thus to start any machine learning aalgorithm we must tailor the data set. First we must remove all columns that have excisive NA value.


# removing all the NA's

training.nona <- rawtraining[, apply(rawtraining, 2, function(x) !any(is.na(x)))]
dim(training.nona)


Next we most remove all values that have low variance with our dependent variable.We can find this by using the nearZeroVar command in R to iterate through the columns; removing all low variance values.


# removing all low variance variables


nzv <- nearZeroVar(training.nona)
filteredtrain <- training.nona[, -nzv]
dim(filteredtrain)


The first 6 columns starting from "X" to "num_window" are only discriptive data, therefore I will remove them from the data set, cause they're are irrelavent in this case


cleaned.training <- filteredtrain[, 7:length(filteredtrain)]


Finally we remove all data that has low correlation from the dependant variable, cause they are very unlikely to have much effect on the outcome.

Correlation

cor <- cor(na.omit(cleaned.training[sapply(cleaned.training, is.numeric)]))

corDescr <- findCorrelation(cor, cutoff = .90)

cleaned.training <- cleaned.training[,-corDescr]
dim(cleaned.training)

#Cross-Validation
Now that we have finished cleaning the data we have to split it into a testing and training data set. 

spliting the data

inTrain <- createDataPartition( y= cleaned.training$classe, p = 0.80 , list = FALSE)
training <- cleaned.training[inTrain,]
testing <- cleaned.training[-inTrain,]


With the two independent data sets, we'll begin creating the decision tree.

modfit <-train(classe~., method ="rpart", data = training)



#creating the graph
fancyRpartPlot(modfit$finalModel)

#Out of Sample Error


Let's now look at the error of our decision tree

#Predictions

predictions1 <- predict(modfit, training)
confusionMatrix(predictions1, training$classe)

Our error for this decision tree is 59%, which is absolutely  horrible.However it's pretty common for a single decision tree to have that low of a error rate.
for a more reliable method we have to turn to the randomforest method.

#randomforest

modfit2 <- randomForest(classe~.,data=training,ntree=100, importance=TRUE)
predictions2 <- predict(modfit2,training)
confusionMatrix(predictions2, training$classe)


As you see our error rate jumped up from 59 to 1 %. This is cause the random forest makes thousands of decisions tree and averages them up together. Creating a more accurate prediction.

#Conclusion

We'll finish up by making our final prediction with the test data.


finalPred <- predict(modfit2,rawtesting)
head(finalPred)
