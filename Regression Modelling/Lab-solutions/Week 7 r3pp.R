#week 7 
#based on Introduction to Statistical Learning by James et al
#and Machine Learning Essentials by Kassambara 
#
#penalised regression
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("glmnet")
library(tidyverse)
library(caret) #with function train
library(glmnet) #with function glmnet
#library(dplyr)
#library(pls)

#load the data
data("Boston", package="MASS")
head(Boston)
#split the data
set.seed(123)
train.samples <- Boston$medv %>% createDataPartition(p=0.7,list=FALSE)
train.data <- Boston[train.samples, ]
head(train.data, 3)
test.data <- Boston[-train.samples, ]
head(test.data, 3)

x <- model.matrix(medv~., train.data)[,-1]
y <- train.data$medv
head(x, 2)
head(Boston, 2)

#with glmnet(x, y, alpha=1, lambda = NULL)

#ridge, alpha=0
#find the best lambda using cv 
set.seed(123)
cv <- cv.glmnet(x,y,alpha=0)
cv
#display the best lambda
cv$lambda.min

model <- glmnet(x,y,alpha=0,lambda=cv$lambda.min)
model
coef(model)

#predict on the test data
x.test <- model.matrix(medv~.,test.data)[,-1]
head(x.test, 2)
predictions <- model %>% predict(x.test) %>% as.vector()

#model performance metrics
#use R2, or 
#use caret::R2 is to make sure to have R2 available, 
#not masked by package pls when pls is used
data.frame(
  RMSE.ridge = RMSE(predictions, test.data$medv),
  Rsquare.ridge = caret::R2(predictions, test.data$medv)
)
 
#lasso, alpha=1
set.seed(123)
cv <- cv.glmnet(x,y,alpha=1)
cv
cv$lambda.min
model <- glmnet(x,y,alpha=1,lambda=cv$lambda.min)
model
coef(model)
x.test <- model.matrix(medv~.,test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
data.frame(
  RMSE.lasso =  RMSE(predictions, test.data$medv),
  Rsquare.lasso = caret::R2(predictions, test.data$medv)
)

#elastic net, using caret which invokes glmnet
#use train with method="glmnet"
set.seed(123)
model <- train(
  medv~., data=train.data, method="glmnet", 
  trControl=trainControl("cv", number=10),
  tuneLength=10
)

model
model$bestTune

#final model; need to specify the best lambda
coef(model$finalModel, model$bestTune$lambda)

x.test <- model.matrix(medv~.,test.data)[,-1]
predictions <- model %>% predict(x.test) 
data.frame(
  RMSE.enet = RMSE(predictions, test.data$medv),
  Rsquare.enet = caret::R2(predictions, test.data$medv)
)

##################################################
#caret package comparison of ridge, lasso, elastic net
#grid range of lambda values
lambda <- 10^seq(-3,3,length=100)

#ridge
set.seed(123)
ridge <- train(
  medv~., data=train.data, method="glmnet", 
  trControl=trainControl("cv", number=9),
  tuneGrid=expand.grid(alpha=0, lambda=lambda)
)

coef(ridge$finalModel, ridge$bestTune$lambda)
predictions <- ridge %>% predict(test.data)

data.frame(
  RMSE.rid = RMSE(predictions, test.data$medv),
  Rsquare.rid = caret::R2(predictions, test.data$medv)
)

#lasso
set.seed(123)
lasso <- train(
  medv~., data=train.data, method="glmnet", 
  trControl=trainControl("cv", number=9),
  tuneGrid=expand.grid(alpha=1, lambda=lambda)
)
  
coef(lasso$finalModel, lasso$bestTune$lambda)
predictions <- lasso %>% predict(test.data)

data.frame(
  RMSE.las = RMSE(predictions, test.data$medv),
  Rsquare.las = caret::R2(predictions, test.data$medv)
)

#elastic
set.seed(123)
elastic <- train(
  medv~., data=train.data, method="glmnet", 
  trControl=trainControl("cv", number=9),
  #tuneLength=9
)

coef(elastic$finalModel, elastic$bestTune$lambda)
predictions <- elastic %>% predict(test.data)

data.frame(
  RMSE.net = RMSE(predictions, test.data$medv),
  Rsquare.net = caret::R2(predictions, test.data$medv)
)

#caret compare all of ridge, lasso, elastic net: 
#choose best with smallest median or mean RMSE
models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
resamples(models) %>% summary(metric="RMSE")


##################################
##################################
#pcr
library(tidyverse)
library(caret)

#install.packages("pls") #has R2 too
library(pls)

set.seed(123)
pcr.model <- train(
  medv~., data=train.data, method="pcr",
  scale=TRUE,
  trControl=trainControl("cv",number=9),
  tuneLength=9
)

plot(pcr.model)
pcr.model$bestTune
summary(pcr.model$finalModel)
pcr.predictions <- pcr.model %>% predict(test.data)

data.frame(
 pcr.RMSE = caret::RMSE(pcr.predictions, test.data$medv),
 pcr.Rsquare = caret::R2(pcr.predictions, test.data$medv)
)

#PLS 
set.seed(123)
pls.model <- train(
  medv~., data=train.data, method="pls",
  scale=TRUE,
  trControl=trainControl("cv",number=9),
  tuneLength=10
)

plot(pls.model)
pls.model$bestTune
summary(pls.model$finalModel)
pls.predictions<-pls.model %>% predict(test.data)
pls.RMSE<-RMSE(pls.predictions, test.data$medv)
pls.Rsquare<-caret::R2(pls.predictions, test.data$medv)

pls.RMSE
pls.Rsquare

#####################  
models2 <- list(ridge=ridge, lasso=lasso, elastic=elastic, pcr=pcr.model, pls=pls.model)
resamples(models2) %>% summary(metric=c("RMSE", "MAE", "Rsquared")) 
#MAE=mean absolute error: smaller means better
