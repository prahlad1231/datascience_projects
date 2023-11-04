# Load the dataset
data <- read.csv("usap.csv")

# Create an interaction term
data$interaction <- data$gr * data$pr

# Fit the regression model
model <- lm(vs ~ gr + pr + interaction, data)

# Print the summary of the regression model
summary(model)
