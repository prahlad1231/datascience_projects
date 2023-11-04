#Reg Week 10 Lab solutions
     
#Question 1a   

library(modelr)
library(MASS)
data(Boston)

set.seed(123)
split <- resample_partition(Boston, c(train=0.7, test = 0.3))
btrain <- data.frame(split$train)
btest <- data.frame(split$test)
head(btest)


#install.packages("xgboost")
library(xgboost)

# xgboost doesn't support data.frame as input. 
# Convert it to matrix first.


#install.packages("data.table")
library(data.table)

#the below code allows us to 'one hot encode' the variables. This is
#necessary for the XGboost model, and basically creates a matrix or array
#of 1s and 0s (binary), where 1 signals yes and 0 signals no.

#This is useful because for categorical variables, we need to know
#whether an observation IS a certain categorical level or not.

#For example, if there is a categorical variable called flowertype,
#and it has the three levels rose, lily and daffodil, then for some
#observation we would put 1 if it is a rose, and 0 if not a rose.

# for more, see 
# https://hackernoon.com/what-is-one-hot-encoding-why-and-when-do-you-have-to-use-it-e3c6186d008f

setDT(btrain) # set data table
setDT(btest)

tr_label_y <- btrain$medv    # y for train set
ts_label_y <- btest$medv     # y for test set

#install.packages("Matrix")
library(Matrix)

#for matrix of x's (excluding y=medv) to prepare to one hot encode:
new_tr_x <- sparse.model.matrix(~.+0, data = btrain[,-c("medv")]) 
# +0 is to remove the intercept; compare the code without +0
# try code new_tr_x[1, ]

new_ts_x <- sparse.model.matrix(~.+0, data = btest[,-c("medv")]) 

#the code xgb.DMatrix will one hot encode the previous matrix. 
#Note that in this case, the dataset we used has no categorical variables. 
#If we use the command
# sapply(btrain, class)
# we can see that all variables are either numeric
#or integer. This means one hot encoding has no purpose because 
#there are no categorical variables to convert out. 

#if we were to change one of the integer variables into a factor, eg. the
#rad variable using the code
# btrain$rad <- as.factor(btrain$rad) 
#then upon repeating the code, the one hot encode would indeed
#expand out the rad factor variable into its levels and we would see
#a different dataset come out as a Dmatrix. 

#we cannot actually see the dataset dtrain. You can either type 
# dtrain and enter to view its information (dimension + some other stuff), 
#or go dim(dtrain) for its dimensions only. 

dtrain <- xgb.DMatrix(data = new_tr_x, label = tr_label_y)
dtest  <- xgb.DMatrix(data = new_ts_x, label = ts_label_y)

###################################################
# XGB
library(xgboost)

xgb <- xgboost(data = dtrain, 
               nrounds = 300, 
               print_every_n = 20, 
               nfold = 10)  # use xgboost package
xgb   #not gb or gbm


XGBpredictions <- predict(xgb, dtest) #xgb with dtest

XGBsummary <- data.frame(RMSE = caret::RMSE(XGBpredictions, btest$medv),
                         RSQ = caret::R2(XGBpredictions, btest$medv),
                         MAE = caret::MAE(XGBpredictions, btest$medv))

XGBsummary 
# to be compared with RFsummary and RFsummary2 to know which is the best

#note the XGB predictions were predicted using the dataframe dtest which
#is the one hot encoded XGboost test dataset. 

#note the XGB model probably did not perform that way due to overfitting.
#The RMSE of the training dataset was EXTREMELY low, which means it has found
#and reduced all of its error from previous iterations (remember that is the 
#goal of an XGB model) so well, that it has become over fitted on the training
#model and its trends. Hence, when we apply it to a new testing dataset,
#it may not perform well all the time.



####################################
#Random forest

#install.packages("randomForest")
library(randomForest)

#install.packages("caret") 
library(caret)  

rfmodel1 <- randomForest(medv ~., 
                         data = btrain,
                         ntree = 100,
                         type = "regression")


tune <- expand.grid(mtry = c(1:15))

rfmodel2 <- train(medv ~., 
                 data = btrain,
                 method = "rf",
                 trControl = trainControl("cv", number = 10),
                 ntree = 100,
                 tuneGrid = tune,
                 importance = TRUE)
#allows us to pre-process the data inside the function 
#in addition to using different methods 
#for our analysis just by making a change to the method =" ".
# caret is more convenient than randomForest
# train might not evaluate the value of mtry that our call to randomForest used. 
#If that's the case, then the results would be different.

 
#######################################################
#Predictions
library(caret)

RFpredictions1 <- predict(rfmodel1, btest)  #rf1 with btest
RFpredictions2 <- predict(rfmodel2, btest)  #rf2 with btest

RFsummary1 <- data.frame(RMSE = RMSE(RFpredictions1, btest$medv),
                         RSQ = caret::R2(RFpredictions1, btest$medv),
                         MAE = MAE(RFpredictions1, btest$medv))
RFsummary1
 
RFsummary2 <- data.frame(RMSE = RMSE(RFpredictions2, btest$medv),
                        RSQ = caret::R2(RFpredictions2, btest$medv),
                        MAE = MAE(RFpredictions2, btest$medv))
RFsummary2
   
###########################################################
#Question 2

#install.packages("mlbench")
library(mlbench)
library(caret)
library(modelr)
data(Soybean)
dim(Soybean)
head(Soybean,2)
View(Soybean)

#There are NAs in the Soybean dataset so we have to omit them.
Soybean <- data.frame(na.omit(Soybean)) #omit NA's
dim(Soybean)

#After we omit some NAs, there will be levels of categorical variables left over
#which are no longer appearing in the training dataset. However, they may appear
#in the test dataset. Hence if you do not enter the following two lines of code,
#the random forest will not create a model since it knows test datasets 
#may contain these lost, non-appearing levels. 

#install.packages("gdata")
library(gdata)
? gdata #or google it online
? drop.levels
 
#install.packages("e1071")
library(e1071) #google it
Soybean <- data.frame(drop.levels(Soybean)) #drop unused levels in a factor
dim(Soybean) #check data cleaned well

###################################
set.seed(123) 

split <- resample_partition(Soybean, c(train = 0.8, test = 0.2))
strain <- data.frame(split$train)
stest <- data.frame(split$test)

#the response Class is categorical below:
#so it's a classification problem, not regression
levels(Soybean$Class) #15 levels
 
tune <- expand.grid(.mtry = c(1:35))   #   .mtry vs mtry

rfmodel <- train(Class ~.,        
                 data = strain,
                 method = "rf",
                 trControl = trainControl("cv", number = 5),
                 ntree = 20,
                 tuneGrid = tune,
                 importance = TRUE)

rfmodel
plot(rfmodel)

RFpredictions <- predict(rfmodel, stest)

#find below the prportion of correctly classified outcomes 
#ie predicted outcomes same as the observed
#(not RMSE etc for a regression model):
 
RFpredictions == stest$Class #see which one is TRUE i.e. corretly classified
mean(RFpredictions == stest$Class) #proportion of TRUE's among all n=113
 
table(RFpredictions, stest$Class) #15 by 15 confusion matrix 
#with more numbers on diagnal, better calssification results
#here Class has 15 levels

#Question 2c)
  
library(caret)
?varImp # see varImp is from caret by varImp {caret} and other info
  
plot(caret::varImp(rfmodel)) #to have caret:: in front of varImp 
#is to make sure to use varImp from caret;
#this shows variable importance for all 15 levels of Class
 
 
#########################################################
## optional below

############ rf  
head(iris)

set.seed(12345)
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3)) 
trainData <- iris[ind==1,] 
testData <- iris[ind==2,] 

#Load Library Random FOrest

library(randomForest)

#Generate Random Forest learning treee

iris_rf <- randomForest(Species~.,data=trainData, ntree=100, proximity=TRUE) 
table(predict(iris_rf), trainData$Species)

##             
##              setosa versicolor virginica
##   setosa         34          0         0
##   versicolor      0         28         3
##   virginica       0          3        29

#Try to print Random Forest model and see the importance features

print(iris_rf) 

#plot(iris_rf)

importance(iris_rf)   

varImpPlot(iris_rf) 
 

#Try to build random forest for testing data

irisPred<-predict(iris_rf,  newdata=testData)
table(irisPred, testData$Species) 

##             
## irisPred     setosa versicolor virginica
##   setosa         16          0         0
##   versicolor      0         19         2
##   virginica       0          0        16

######################################################
# neural network

# install.packages("neuralnet")
# install.packages("nnet")
library(neuralnet)
library(nnet)

# Split data
train_idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris_train <- iris[train_idx, ]
iris_test  <- iris[-train_idx, ]

# Binary classification 
bb <- neuralnet(Species == "setosa" ~ Petal.Length + Petal.Width, iris_train, linear.output = FALSE)
pred_bb <- predict(bb, iris_test)
table(iris_test$Species == "setosa", pred[, 1] > 0.5)

# Multiclass classification
mm <- neuralnet((Species == "setosa") + (Species == "versicolor") + (Species == "virginica")
                  ~ Petal.Length + Petal.Width, iris_train, linear.output = FALSE)  
pred_mm <- predict(mm, iris_test) 
table(iris_test$Species, apply(pred, 1, which.max))
  
# https://rpubs.com/vitorhs/iris
# https://rpubs.com/jkw552403/283196
# https://rpubs.com/ChristianLopezB/Supervised_Machine_Learning
# http://topepo.github.io/caret/index.html
# https://rpubs.com/thirus83/451573

 