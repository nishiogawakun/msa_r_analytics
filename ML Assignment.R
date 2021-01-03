library(caret)
library(DataExplorer)
library(e1071)
library(kernlab)
library(randomForest)
library(tidyverse)
library(WVPlots)
library(dplyr)
dataset <- read.csv("PLoan_randomized.csv")
colnames(dataset) <- c("Age","Salary","Own","Size","Outcome")


introduce(dataset)
#### Question 1
## A quick summary of the data show that there are 119 rows and 5 columns, two which are discrete 
## and three that are continuous.  Luckily there are no missing values, so we don't have to worry 
## about that.  Since we only have 5 fields to work with, each that seems pretty relevant on the face 
## of it, I don't foresee the need to do any major dimension reduction.  As a result, I am curious 
## about the results of the LDA model.  I also don't see this as a clustering problem, but rather a pure 
## play classification issue, so I am skeptical about the accuracy of the kNN model.  From what I've 
## read SVM's only work with numeric data so I imagine that we'll be deploying dummy variables.  Combine 
## that with the tricks that SVM has to play to create linearly separable data, and I think the SVM model 
## might not be able to beat the CART model.

dataset %>% group_by(Outcome) %>% tally()
#### Question 2
## Due to the fact that the majority of outcome values are "Yes", I would expect by chance a simple majority 
##classifier to classify additional records as "Yes" more often than no.

set.seed(7)
validation_index <- createDataPartition(dataset$Age, p=0.80, list=FALSE,)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Linear Discriminant Analysis
fit.lda <- train(Outcome~., data=dataset, method="lda", metric=metric, trControl=control)
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Outcome)

# CART
set.seed(7)
fit.cart <- train(Outcome~., data=dataset, method="rpart", metric=metric, trControl=control)
predictions <- predict(fit.cart, validation)
confusionMatrix(predictions, validation$Outcome)
print(fit.cart)

# kNN
set.seed(7)
fit.knn <- train(Outcome~., data=dataset, method="knn", metric=metric, trControl=control)
predictions <- predict(fit.knn, validation)
confusionMatrix(predictions, validation$Outcome)

# SVM
set.seed(7)
fit.svm <- train(Outcome~., data=dataset, method="svmLinear", metric=metric, trControl=control)
predictions <- predict(fit.svm, validation)
confusionMatrix(predictions, validation$Outcome)
print(fit.svm)

#### Question 3
## LDA: 0.7143
## CART: 1
## kNN: 0.8571
## SVM: 0.8095
 
#### Question 4
## LDA
# Prediction no yes
# no   0   2
# yes  4  15
## CART:
# Prediction no yes
# no   4   0
# yes  0  17
## kNN:
# Prediction no yes
# no   1   0
# yes  3  17
## SVM:
# Prediction no yes
# no   0   0
# yes  4  17

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm))
summary(results)

dotplot(results)

#### Question 5
## With an accuracy of 100% the CART model proved the most accurate of the four models tested.  
## I personally did not expect it to get 100% of the cases correct, but nonetheless, it's 
## impressive.  It is my guess, similar to what was stated above, the presence of categorical 
## variables and the relatively small number of fields enabled the CART model to really excel.  
## I also have to wonder if the training data is built on natural human heuristics that exist 
## with the loan officers that the CART model might inherently mimic.

#### Question 6
## With an accuracy of 100% in our small set of data, I think you would naturally be inclined 
## to use the CART model for predictions.  However, for signature loans (which I assume are 
## loans with no collateral) are pretty risky, and I would hesitate using an simple ML algorithm 
## without training and validating it with a MUCH larger sample size.  

#### Question 7
## In comparison, I feel like car loans have much fewer variables.  Or at least, the variables might
## be much more apparent.  Type of vehicle, amount, year, etc. provide nice clean data for a model 
## to use in conjunction with the applicant's information.  I personally feel I'd be more inclined to 
## rely on ML for this type of application first.
