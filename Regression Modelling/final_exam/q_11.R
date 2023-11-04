# Load the required library
install.packages("mlbench")
library(mlbench)

# Load the PimaIndiansDiabetes2 dataset and remove missing values
data(PimaIndiansDiabetes2)
diabetes_data <- na.omit(PimaIndiansDiabetes2)

# Fit a logistic model using age as the predictor
model_age <- glm(diabetes ~ age, data = diabetes_data, family = binomial)

# Calculate AIC for the model
aic_age <- AIC(model_age)

# Fit a logistic model using age, mass, and pressure as predictors
model_age_mass_pressure <- glm(diabetes ~ age + mass + pressure, data = diabetes_data, family = binomial)

# Calculate AIC for the model
aic_age_mass_pressure <- AIC(model_age_mass_pressure)

# Create data for prediction
new_data <- data.frame(age = 26, mass = 31, pressure = 50)

# Use the better model to predict the probability for the new data
if (aic_age < aic_age_mass_pressure) {
  predicted_prob <- predict(model_age, newdata = new_data, type = "response")
} else {
  predicted_prob <- predict(model_age_mass_pressure, newdata = new_data, type = "response")
}

# Round AIC values and predicted probability to two decimal places
aic_age <- round(aic_age, 2)
aic_age_mass_pressure <- round(aic_age_mass_pressure, 2)
predicted_prob <- round(predicted_prob, 2)

# Print the results
cat("(1) AIC value with age as the predictor:", aic_age, "\n")
cat("(2) AIC value with age, mass, and pressure as predictors:", aic_age_mass_pressure, "\n")
cat("(3) Predicted probability to have diabetes:", predicted_prob, "\n")
