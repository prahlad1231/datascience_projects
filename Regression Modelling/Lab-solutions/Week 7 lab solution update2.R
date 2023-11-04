#week 7 
 
# load the libraries
library(tidyverse)
library(glmnet)
library(caret)

attach(mtcars) 
head(mtcars, 3)
dim(mtcars)
? mtcars

# split data
set.seed(123)
mtcars_train <- mtcars %>% sample_frac(0.7) #for 70%
# or select first 22 observations as below
# mtcars_train <- mtcars[1:22, ]
mtcars_test <- mtcars %>% setdiff(mtcars_train)
dim(mtcars_train)
dim(mtcars_test)

#or
train_index <- sample(1:nrow(mtcars),  nrow(mtcars)*0.7)
mtcars_train <- mtcars[train_index ,  ]
mtcars_test  <- mtcars[-train_index ,  ]
dim(mtcars_train)
dim(mtcars_test)

# look at their first 3 values 
head(mtcars_train, 3)
head(mtcars_test, 3)
head(mtcars,3)

# train set to get the dependent variable
y_train <- mtcars_train$mpg

# train set to get the independent variables
x_train <- model.matrix(mpg~. , data=mtcars_train)[, c(4:6)]

# test set  
y_test <- mtcars_test$mpg
x_test <- model.matrix(mpg~. , data=mtcars_test)[, c(4:6)]

#look at their 3 values
y_train[1:3]
x_train[1:3, ]
y_test[1:3]
x_test[1:3, ]

# set the range of lambda values, to find best lambda
# lambda_seq <- 10^seq(-2, 2, by = 0.1)
lambda_seq <- 10^seq(-2, 2, length = 100)

# use cross validation glmnet
set.seed(123) # we need it as nfolds may be randomly done
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambda_seq, nfolds = 5)
ridge_cv
# note: 
# ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0) also works
# when we don't choose lambda =lambda_seq or nfolds=5
# their default lambda = NULL and nfolds=10 will be used
# for more info use ?cv.glmnet


# best lambda value, chosen from 2 options
best_lambda <- ridge_cv$lambda.min
best_lambda

# use glmnet function to train the ridge regression 
ridge_fit <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)
# check the model
summary(ridge_fit)
# find slope estimates
coef(ridge_fit)

# make prediction
ridge_pred <- predict(ridge_fit, s = best_lambda, newx = x_test)
head(ridge_pred)

# calculate metrics to evaulate models/predictions
library(caret)
data.frame(
  ridge_rmse = RMSE(y_test, ridge_pred),
  ridge_mae = MAE(y_test, ridge_pred),
  ridge_r2 = caret::R2(y_test, ridge_pred)
)
 



################################################

# lasso: find best lamda
set.seed(123)
cv_output <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambda_seq, nfolds = 5)
cv_output
best_lam <- cv_output$lambda.min
best_lam

# build the lasso model with best lamda value found
best_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam)
coef(best_lasso)

# predict
lasso_pred <- predict(best_lasso, s = best_lam, newx = x_test)
head(lasso_pred)

#metrics
library(caret)
data.frame(
  lasso_rmse = RMSE(y_test, lasso_pred),
  lasso_mae = MAE(y_test, lasso_pred),
  lasso_r2 = caret::R2(y_test, lasso_pred)
)


################################################################
####### swiss
# load the library and data
library(glmnet)
data(swiss)

# prepare y and x, and lambda range
y_var <- swiss$Fertility
x_vars <- model.matrix(Fertility~. , data=swiss)[, -1]
head(x_vars )

# split the data into train and test sets
# set seed for reproductivity
set.seed(123)
# train.sam <- sample(1:nrow(x_vars),  nrow(x_vars)*0.7)
train.sam <-1:40
y_train <- y_var[train.sam]
x_train <- x_vars[train.sam,     ]
y_test <- y_var[-train.sam]
x_test <- x_vars[-train.sam,]

########## 
lambda_seq <- 10^seq(-3, 3, length=100)

# ridge: find best lamda
set.seed(123)
cv_output <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambda_seq, nfolds = 5)
plot(cv_output)
best_lam <- cv_output$lambda.min
best_lam

# build the model with best lamda value 
best_ridge <- glmnet(x_train, y_train, alpha = 0, lambda = best_lam)
coef(best_ridge)

# predict
ridge_pred <- predict(best_ridge, s = best_lam, newx = x_test)
head(ridge_pred)

# evaluate
library(caret)
data.frame(
ridge_rmse = RMSE(y_test, ridge_pred),
ridge_mae = MAE(y_test, ridge_pred),
ridge_r2 = caret::R2(y_test, ridge_pred)
)

######
set.seed(123)
# lasso: find best lamda
cv_output <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambda_seq, nfolds = 5)
cv_output
best_lam <- cv_output$lambda.min
best_lam

# build the model with best lamda value 
best_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam)
coef(best_lasso)

# pred
lasso_pred <- predict(best_lasso, s = best_lam, newx = x_test)
head(lasso_pred)

# evaluate
library(caret)
data.frame(
  lasso_rmse = RMSE(y_test, lasso_pred),
  lasso_mae = MAE(y_test, lasso_pred),
  lasso_r2 = caret::R2(y_test, lasso_pred)
)

##################################################