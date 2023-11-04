# Load the required library
library(rpart)

# Load the iris dataset
data(iris)

# Scale the four numerical columns
iris[,1:4] <- scale(iris[,1:4])

# Use the first 130 rows to build a decision tree
fit <- rpart(Species ~ ., data = iris[1:130,], method = "class")

# Print the decision tree
printcp(fit)

# Plot the decision tree
plotcp(fit)
