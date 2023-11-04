#Week 9 lab solutions update

#Question 1a

library(datasets)
data(mtcars)

library(caret)
library(tidyverse)
library(modelr)
 
set.seed(123) #important to have re-produciable results (same random results again)
split <- resample_partition(mtcars, c(train=0.7, test = 0.3))

cartrain <- data.frame(split$train)
cartest <- data.frame(split$test)

#note
#can be done differently:
set.seed(123)
train.samples <- mtcars$mpg %>% createDataPartition(p=0.7,list=FALSE)
train_data <- mtcars[train.samples,]
test_data <- mtcars[-train.samples,]

#use sample_frac or sample too
? sample_frac
? sample
###################################################

cartrain <- data.frame(split$train)
cartest <- data.frame(split$test)

#Question 1b

library(caret)

lm <- lm(mpg ~., 
         data = cartrain)
lm
summary(lm)

#another option

lm3 <- train(mpg ~ ., 
             data = cartrain,
             method = "lm",
             trControl = trainControl("cv", number = 10),
             #preProcess=c("center", "scale"),   
             #"center" subtracts the mean of the predictor's data 
             #(again from the data in x) from the predictor values 
             #while "scale" divides by the standard deviation
             tuneLength = 20
)

lm2 #this may change on different cv numbers etc
summary(lm2) #remains unchanged, the same as summary(lm)

#Question 1c

lmpredictions <- predict(lm, cartest)

#Question 1d

lmsummary <- data.frame(
  RMSE = RMSE(lmpredictions, cartest$mpg),
                        RSQ = caret::R2(lmpredictions, cartest$mpg),
                        MAE = MAE(lmpredictions, cartest$mpg))

lmsummary

#Question 1e: using a knn model instead of a linear model

knn <- train(mpg ~ ., 
                       data = cartrain,
                       method = "knn",
                       trControl = trainControl("cv", number = 10), 
                       preProcess=c("center", "scale"),   
                       #"center" subtracts the mean of the predictor's data 
                       #(again from the data in x) from the predictor values 
                       #while "scale" divides by the standard deviation
                       tuneLength = 20
)

knn
plot(knn)

#knn predict
knnpredictions <- predict(knn, cartest)

knnsummary <- data.frame(RMSE = RMSE(knnpredictions, cartest$mpg),
                         RSQ = caret::R2(knnpredictions, cartest$mpg),
                         MAE = MAE(knnpredictions, cartest$mpg))
knnsummary

#Question 1f: using a decision tree instead of a linear or knn model
library(rpart)
dtree <- train(mpg ~ ., 
             data = cartrain,
             method = "rpart",
             trControl = trainControl("cv", number = 10), 
             #preProcess=c("center", "scale"), 
             #"center" subtracts the mean of the predictor's data 
             #(again from the data in x) from the predictor values 
             #while "scale" divides by the standard deviation
             tuneLength = 20 
)

dtree
plot(dtree)
 
#install.packages("rattle")
library(rattle)
fancyRpartPlot(dtree$finalModel)

#check
mean(cartrain$mpg) #returns 20 ??? the value in the root node
length(cartrain$mpg) #returns 22

mean(cartrain$mpg[cartrain$cyl>4.999])
length(cartrain$mpg[cartrain$cyl>4.99])
length(cartrain$mpg[cartrain$cyl>4.99])/length(cartrain$mpg)

mean(cartrain$mpg[cartrain$cyl<5])
length(cartrain$mpg[cartrain$cyl<5])

#predict mpg on test set
dtreepredictions <- predict(dtree, cartest)

dtreesummary <- data.frame(RMSE = RMSE(dtreepredictions, cartest$mpg),
                         RSQ = caret::R2(dtreepredictions, cartest$mpg),
                         MAE = MAE(dtreepredictions, cartest$mpg))
dtreesummary

#Question 1g

rbind(lmsummary, knnsummary, dtreesummary)

#####################################################
#Question 2a
library(tidyverse)
library(caret)
library(modelr)
library(MASS)
data(Boston)

set.seed(12345)
split <- resample_partition(Boston, c(train=0.8, test = 0.2))

btrain <- data.frame(split$train)
btest <- data.frame(split$test)

#Question 2 continued: knn model

knn <- train(medv ~ ., 
             data = btrain,
             method = "knn",
             trControl = trainControl("cv", number = 10), 
             preProcess=c("center", "scale"), 
             tuneLength = 20
)

knn
plot(knn)

knnpredictions <- predict(knn, btest)

knnsummary <- data.frame(RMSE = RMSE(knnpredictions, btest$medv),
                         RSQ = caret::R2(knnpredictions, btest$medv),
                         MAE = MAE(knnpredictions, btest$medv))
knnsummary

#Question 2 continued: decision tree model

dtree <- train(medv ~ ., 
               data = btrain,
               method = "rpart",
               trControl = trainControl("cv", number = 10), 
               preProcess=c("center", "scale"), 
               tuneLength = 20
)

dtree
plot(dtree)

library(rattle)

fancyRpartPlot(dtree$finalModel)

dtreepredictions <- predict(dtree, btest)

dtreesummary <- data.frame(RMSE = RMSE(dtreepredictions, btest$medv),
                           RSQ = caret::R2(dtreepredictions, btest$medv),
                           MAE = MAE(dtreepredictions, btest$medv))
dtreesummary
knnsummary

#############################################
#Question 3

data(iris)

split <- resample_partition(iris, c(train=0.75, test = 0.25))

itrain <- data.frame(split$train)
itest <- data.frame(split$test)

knn <- train(Sepal.Length ~ ., 
             data = itrain,
             method = "knn",
             trControl = trainControl("cv", number = 10), 
             preProcess=c("center", "scale"), 
             tuneLength = 20
)

knn
plot(knn)

knnpredictions <- predict(knn, itest)

knnsummary <- data.frame(RMSE = RMSE(knnpredictions, itest$Sepal.Length),
                         RSQ = caret::R2(knnpredictions, itest$Sepal.Length),
                         MAE = MAE(knnpredictions, itest$Sepal.Length))
knnsummary

####################################################
#Question 2 continued: decision tree model

dtree <- train(Sepal.Length ~ ., 
               data = itrain,
               method = "rpart",
               trControl = trainControl("cv", number = 10), 
               #preProcess=c("center", "scale"), 
               tuneLength = 20
)

dtree
plot(dtree)

library(rattle)

fancyRpartPlot(dtree$finalModel)

#check
mean(itrain$Sepal.Length)
length(itrain$Sepal.Length)

mean(itrain$Sepal.Length[itrain$Petal.Length<4.3])
length(itrain$Sepal.Length[itrain$Petal.Length<4.3])
length(itrain$Sepal.Length[itrain$Petal.Length<4.3])/length(itrain$Sepal.Length)


dtreepredictions <- predict(dtree, itest)

dtreesummary <- data.frame(RMSE = RMSE(dtreepredictions, itest$Sepal.Length),
                           RSQ = caret::R2(dtreepredictions, itest$Sepal.Length),
                           MAE = MAE(dtreepredictions, itest$Sepal.Length))
dtreesummary

knnsummary


