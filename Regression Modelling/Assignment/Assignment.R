install.packages("ISLR")

library(ISLR)
library(ggplot2)
library(gridExtra)
library(caret)
library(glmnet)

hitters = ISLR::Hitters

summary(hitters)
colnames(hitters)

sum(is.na(hitters))

# remove all missing values
hitters = na.omit(hitters)
sum(is.na(hitters))



# Create a histogram for the "Salary" variable
hist_plot <- ggplot(hitters, aes(x = Salary)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(title = "Salary Histogram", x = "Salary", y = "Frequency")

# Create a boxplot for the "Salary" variable
box_plot <- ggplot(hitters, aes(y = Salary)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Salary Boxplot", y = "Salary")

grid.arrange(hist_plot, box_plot, ncol=2)

# 3. Prepare inputs for glmnet (ML syntax "X, y" instead of formula y ~ x)
x = model.matrix(Salary ~ . - 1, data = hitters)  # Matrix, excluding Salary
y = hitters$Salary

# 4. Fit a single model. Default: Lasso (alpha=1)
fit = glmnet(x, y, alpha = 1)  # glmnet performs multiple fits with a lambda grid
length(fit$lambda)
# Fetch a middle model, display Lambda and coefficients:
print(paste("4. Lambda =", fit$lambda[40]))
coef(fit)[, 40]
# Lab: Try different parameters

# 5. Fit various model types:
set.seed(1)
fit.ridge = glmnet(x, y, alpha = 0)   # Ridge Regression
fit.lasso = glmnet(x, y)              # Lasso
fit.elnet1 = glmnet(x, y, alpha = 0.1)  # 1st Elastic Net
fit.elnet9 = glmnet(x, y, alpha = 0.9)  # 2nd Elastic Net

# 6. Visualize the shrinking of coefficients
par(mfrow = c(2, 2))
plot(fit.ridge, xvar = "lambda", main = "Ridge (alpha=0)")
plot(fit.lasso, xvar = "lambda", main = "Lasso (alpha=1)")
plot(fit.elnet1, xvar = "lambda", main = "Elastic Net (alpha=0.1)")
plot(fit.elnet9, xvar = "lambda", main = "Elastic Net (alpha=0.9)")
# Lab: Plot L1 Norm (default: xvar="norm")
# Lab: Plot Fraction deviance explained (xvar="dev")

# 7. Create training and test datasets
x = model.matrix(Salary ~ ., hitters)[, -1]
grid = 10^seq(10, -2, length = 100)  # Create a grid of Lambda from 0.01 to 10^10
set.seed(1)  # Generate reproducible training and test data
train = sample(1:nrow(x), nrow(x) / 2)  # Training dataset 50%
test = (-train)
y.test = y[test]

# 8. Find the optimal Lambda through Cross-Validation
set.alpha = 1  # Lasso
set.seed(1)
fit.train = glmnet(x[train, ], y[train], alpha = set.alpha, lambda = grid)  # Grid
cv.train = cv.glmnet(x[train, ], y[train], alpha = set.alpha)
par(mfrow = c(1, 1))
plot(cv.train)
bestlam.cv = cv.train$lambda.min  # Lambda with the smallest standard error
print(paste("8. Best Lambda =", bestlam.cv))
pred.test = predict(fit.train, s = bestlam.cv, newx = x[test, ])
print(paste("8. mse =", mean((pred.test - y.test)^2)))
out = glmnet(x, y, alpha = set.alpha)
# Fit with the entire dataset, grid
# Show coefficients with optimal Lambda:
predict(out, typ = "coefficients", s = bestlam.cv)[1:20,]  # [] is optional
# Lab: Try different alphas. Minimize mse.