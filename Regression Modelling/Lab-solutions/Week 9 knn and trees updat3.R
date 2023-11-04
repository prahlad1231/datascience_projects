#KNN for regression

library(caret) #for the data
library(tidyverse)
library(modelr)

#the dataset we will be using for a number of examples is the Sacramento
#dataset from the package _caret_

data(Sacramento)
dim(Sacramento)
split <- resample_partition(Sacramento, c(train = 0.8, test = 0.2))
Sactrain <- data.frame(split$train)
Sactest <- data.frame(split$test)

# The above commands split the Sacramento dataset from the caret package 
# into a training dataset, 80% of the original, and a testing dataset, 
# 20% of the original

knnregression <- train(price ~ ., 
                  data = Sactrain,
                  method = "knn",
                  trControl = trainControl("cv", number = 10), 
                  #trControl states that we will cross validate 10 times 
                  #preProcess=c("center", "scale"), 
                  #"center" subtracts the mean of the predictor's data 
                  #(again from the data in x) from the predictor values 
                  #while "scale" divides by the standard deviation. This only
                  #works when all variables are numerical (which is not the case 
                  #here)
                  tuneLength = 20
                  #tuneLength specifies how many values of k we want the model 
                  #to test. Increasing this number will increase 
                  #computational complexity 
                  )

plot(knnregression)
 
knnregression

SacpredictionsR <- predict(knnregression, Sactest)

sqrt(mean((Sactest$price - SacpredictionsR)^2))

RMSE(SacpredictionsR, Sactest$price)

#KNN for classification
install.packages("e1071")
library(e1071) #this library is required for classification

knnclassification <- train(type ~.,
                           data = Sactrain,
                           method = "knn",
                           trControl = trainControl("cv", number = 10),
                           tuneLength = 20)

plot(knnclassification)

knnclassification

SacpredictionsC <- predict(knnclassification, Sactest)

mean(SacpredictionsC == Sactest$type)

#Decision trees

library(caret)
library(tidyverse)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
library(modelr)

split <- resample_partition(iris, c(train = 0.8, test = 0.2))
train <- data.frame(split$train)
test <- data.frame(split$test)

cartmodel <-  rpart(Species ~.,
                    data = train,
                    method = "class")

cartmodel
rpart.plot(cartmodel, type = 4, extra = 2)

#Decision trees: pruning them

cartmodel2 <- train(Species ~.,
                   data = train,
                   method = "rpart",
                   trControl = trainControl("cv", number = 10),
                   tuneLength = 25)
cartmodel2

#comparison between our original model, cartmodel, 
#and our new model, cartmodel2:

cm <- predict(cartmodel, test, type = "class") 
#notice we now include a type equal to class, 
#because we are predicting the class of an observation, 
#not a prob, or vector, or matrix 
#(because of the rpart code to train the model)

cm2 <- predict(cartmodel2, test, type = "raw")  
# here type must be either "raw" or "prob" 
#(because of the train code to train the model)
cm2p <- predict(cartmodel2, test, type = "prob")
#150      0 0.03030303 0.9696970
#here 0.9696970 is 32/33 as given by the plot

# carmodel and carmodel2 both with prob will produce the same output
cmp  <- predict(cartmodel,  test, type = "prob") 
cmp
cm2p <- predict(cartmodel2, test, type = "prob") 
cm2p
 
#accuracy
mean(cm == test$Species)
mean(cm2 == test$Species)
mean(cm2p == test$Species) # not ok, due to the prob's

#visualising cart trees from caret requires a different package (since it is
#no longer an rpart object).    

library(rattle)

fancyRpartPlot(cartmodel2$finalModel)



