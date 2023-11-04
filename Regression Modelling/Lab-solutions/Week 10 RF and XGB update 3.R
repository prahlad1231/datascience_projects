#Week 10 RF and XGB
 
library(caret)
library(tidyverse)
library(modelr)
#install.packages("randomForest")
library(randomForest)


data("Sacramento")
 
# This splits the Sacramento dataset from the caret package into 
# a training dataset, 80% of the original, and 
# a testing dataset,  20% of the original

split <- resample_partition(Sacramento, c(train = 0.8, test = 0.2))
Sactrain <- data.frame(split$train)
Sactest <- data.frame(split$test)

# this builds a random forest model, where price is predicted by the variables
# city, beds, baths, sqft and type. One thing you will notice is, if you try
# to fit this random forest model using the actual random forest package, and
# include ALL variables (eg. you change the formual to price ~.), you will
# receive an error message saying you cannot use variables with more than 53 
# levels. This is because random forest models will actually change a lot of 
# categorial variables such that each level within a categorical variable 
# becomes a "new" variable. If we recall what a random forest does, or uses, 
# each decision tree has to select a variable to select for a split, and when 
# exceeding 53 variables, the computational complexity becomes too great to 
# handle, unfortunately. 

rfmodel <- randomForest(price ~ city + beds + baths + sqft + type,
                        data = Sactrain,
                        ntree = 100,
                        type = "regression")

#using the caret package, there is a workaround so it is possible. 
#However, in the example below, we will use the same variables as above 

#we can also tune the mtry hyperparameter
  
tune <- expand.grid(.mtry = c(1:35))

rfmodel2 <- train(price ~ city + beds + baths + sqft + type,
                 data = Sactrain,
                 method = "rf",
                 trControl = trainControl("cv", number = 10),
                 ntree = 50,
                 tuneGrid = tune,
                 importance = TRUE)

plot(varImp(rfmodel2)) #this shows variable importance              

#caret cannot tune other hyperparameters besides mtry,
#however we can manually set them 
                 
rfmodel3 <- train(price ~ city + beds + baths + sqft + type,
                 data = Sactrain,
                 method = "rf",
                 trControl = trainControl("cv", number = 10),
                 ntree = 10, 
                 nodesize = 6, #min node size
                 maxnodes = 10, #max node size
                 tuneGrid = tune,
                 importance = TRUE)

plot(varImp(rfmodel3))

#tuning the hyperparameters of the random forest in caret is below: we first
#specify a grid of values we want our hyperparameters to take.
#Caret then trains the models on all of these parameters to tune them
#and find the best combinations of them. 

#This will take more time! You are increasing computational complexity#

rfgrid <- expand.grid(.mtry = 1:15)

#in trControl, we need to specify a search to be grid, 
#because we are saying we want the model to try out 
#all our values in the rfgrid to tune its parameters

rftune_model <- train(price ~., 
                data = Sactrain,
                method = "rf",
                trControl = trainControl("repeatedcv", number = 10, repeats = 3),
                ntree=50,
                importance = TRUE, 
                tuneGrid = rfgrid)


###########################################
#boosted model
  
library(caret)
library(tidyverse)
library(modelr)

#install.packages("xgboost")
library(xgboost)
#install.packages("gbm")
library(gbm) 


split <- resample_partition(Sacramento, c(train = 0.8, test = 0.2))
Sactrain <- data.frame(split$train)
Sactest <- data.frame(split$test)

gbm<- train(price ~., 
                data = Sactrain,
                method =  "gbm",
                mtry=30,
                trControl = trainControl("cv", number = 10)
                )


##########################################
#install.packages("ISLR")
library(ISLR)
library(tidyverse) 

ml_data <- College
ml_data %>%   glimpse()

library(caret)
# Partition into training and test data
set.seed(42)
index <- createDataPartition(ml_data$Private, p = 0.7, list = FALSE)
train_data <- ml_data[index, ]
test_data  <- ml_data[-index, ]

model_gbm <- caret::train(Private ~ .,
                          data = train_data,
                          method = "gbm",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 5, 
                                                   repeats = 3, 
                                                   verboseIter = FALSE),
                          verbose = 0)

model_gbm

#######################################
caret::confusionMatrix(
  data = predict(model_gbm, test_data),
  reference = test_data$Private
)


#############################
#XGBoost

library(xgboost)
xgboost_model <- xgboost(data = as.matrix(train_data[, -1]), 
                         label = as.numeric(train_data$Private)-1,
                         max_depth = 3, 
                         objective = "binary:logistic", 
                         nrounds = 10, 
                         verbose = FALSE,
                         prediction = TRUE)
xgboost_model

predict(xgboost_model, as.matrix(test_data[, -1])) %>% 
        as.tibble() %>%
        mutate(prediction = round(value),
        label = as.numeric(test_data$Private)-1) %>%
        count(prediction, label)
        
# for more info, see below
# https://www.r-bloggers.com/2018/11/machine-learning-basics-gradient-boosting-xgboost/
  
 
#############################
#XGBoost option 2

xgbtree <- train(price ~., 
                data = Sactrain,
                method = "xgbTree", #check "gbm"
                objective ="reg:squarederror", #loss function
                trControl = trainControl("cv", number = 10)
                )



#################################
# optional below

library(xgboost)
#install.packages("data.table")
library(data.table) 

setDT(Sactrain) #this coerces the previous data frame into a data table
setDT(Sactest)

#the below code allows us to 'one hot encode' the variables. This is
#necessary for the XGboost model, and basically creates a matrix or array
#of 1s and 0s (binary), where 1 signals yes and 0 signals no.

#This is useful because for categorical variables, we need to know
#whether an observation IS a certain categorical level or not.

#For example, if there is a categorical variable called flowertype,
#and it has the three levels rose, lily and daffodil, then for some
#observation we would a put 1 if it is a rose, and 0 if not a rose.

# for more, see 
# https://hackernoon.com/what-is-one-hot-encoding-why-and-when-do-you-have-to-use-it-e3c6186d008f

labels <- Sactrain$price
ts_label <- Sactest$price

#install.packages("Matrix")
library(Matrix)

new_tr <- sparse.model.matrix(~.+0, data = Sactrain[,-c("price")])
new_ts <- sparse.model.matrix(~.+0, data = Sactest[,-c("price")])
 
dtrain <- xgb.DMatrix(data = new_tr, label = labels)
dtest <- xgb.DMatrix(data = new_ts, label = ts_label)

xgb <- xgboost(data = dtrain, 
               nrounds = 2000, 
               print_every_n = 1000, 
               nfold = 10)

My.Pred<-predict(xgb,  newdata=dtest)
 
