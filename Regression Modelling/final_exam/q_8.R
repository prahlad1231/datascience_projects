# Load the required library
library(glmnet)

# Load the Longley dataset
data(longley)

# Use the first 12 observations as your training set and the remaining as your test set
train <- longley[1:12,]
test <- longley[13:16,]

# Fit a lasso model using glmnet
x_train <- model.matrix(Employed~., data = train)[,-1]
y_train <- train$Employed
x_test <- model.matrix(Employed~., data = test)[,-1]
y_test <- test$Employed

fit <- glmnet(x_train, y_train, alpha = 1, lambda = 0.01)

# Get the coefficients
coef(fit)

# Predict on the test set
predictions <- predict(fit, newx = x_test)
predictions
# Calculate RMSE
rmse <- sqrt(mean((y_test - predictions)^2))
rmse
# Calculate MAE
mae <- mean(abs(y_test - predictions))
mae

# Predict for the year 1959
pred_1959 <- predict(fit, newx = x_test[which(test$Year == 1959),])
pred_1959
# Calculate the residual for 1959
residual_1959 <- y_test[which(test$Year == 1959)] - pred_1959
residual_1959
# Calculate the sum of residuals for the years 1959-1962
sum_residuals <- sum(y_test - predictions)
sum_residuals


#####################

# Load the longley data set
# Load the longley dataset
data(longley)

# Split the data into a training set (first 12 observations) and a test set (remaining observations)
training_set <- longley[1:12, ]
test_set <- longley[13:16, ]

# Load the glmnet library
library(glmnet)

# Prepare the data for glmnet
x_train <- as.matrix(training_set[, -6])  # Exclude the "Year" variable
y_train <- training_set$Employed
x_test <- as.matrix(test_set[, -6])  # Exclude the "Year" variable
y_test <- test_set$Employed

# Fit a Lasso model using glmnet with lambda = 0.01
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = 0.01)

# Predict Employed on the test set
lasso_pred <- predict(lasso_model, newx = x_test)

# Calculate RMSE and MAE
rmse <- sqrt(mean((lasso_pred - y_test)^2))
mae <- mean(abs(lasso_pred - y_test))

# Predict Employed for the year 1959
x_1959 <- as.matrix(longley[13, -6])  # Data for the year 1959
employed_1959 <- predict(lasso_model, newx = x_1959)

# Calculate residuals
residuals <- as.vector(lasso_pred - y_test)
sum_residuals <- sum(residuals)

# Print the results with 2 decimal places
cat("The estimated intercept is", round(lasso_model$a0, 2), "\n")
cat("The estimated slope for Population is", round(coef(lasso_model)[6], 2), "\n")
cat("The RMSE value for the fitted model on the test set is", round(rmse, 2), "\n")
cat("The MAE value for the fitted model on the test set is", round(mae, 2), "\n")
cat("The predicted value of Employed for the year 1959 is", round(employed_1959, 2), "\n")
cat("The residual for the year 1959 is", round(residuals[1], 2), "\n")
cat("The sum of the residual values for the 4 years 1959-1962 is", round(sum_residuals, 2), "\n")



# Load the required library
library(glmnet)

# Load the Longley dataset
data(longley)

# Define the predictors and the response variable
predictors <- longley[,c("GNP.deflator", "GNP", "Unemployed", "Armed.Forces", "Population", "Year")]
response <- longley[,"Employed"]

# Split the data into training and test sets
train_data <- predictors[1:12,]
test_data <- predictors[13:16,]
train_labels <- response[1:12]
test_labels <- response[13:16]

# Fit the lasso model
fit <- glmnet(as.matrix(train_data), train_labels, alpha = 1, lambda = 0.01)

# Get the coefficients
coef(fit)

# Predict on the test set
predictions <- predict(fit, newx = as.matrix(test_data))
predictions
# Calculate RMSE
rmse <- sqrt(mean((predictions - test_labels)^2))
rmse
# Calculate MAE
mae <- mean(abs(predictions - test_labels))
mae
# Calculate residuals for 1959
residual_1959 <- test_labels["1959"] - predictions["1959"]
residual_1959
# Calculate sum of residuals for 1959-1962
sum_residuals <- sum(test_labels - predictions)
sum_residuals
