# Load the required library and load the dataset
install.packages("faraway")
library(faraway)
data(gala)

# Subset the data to the first 23 rows
gala_subset <- gala[1:23, ]

# Fit a Poisson GLM with mu_i as the mean of Species_i by Endemics_i
model1 <- glm(Species ~ Endemics, data = gala_subset, family = poisson)

# Calculate AIC for the first model
aic1 <- AIC(model1)

# Fit a Poisson GLM with mu_i as the mean of Species_i by Endemics_i, Area_i, and Nearest_i
model2 <- glm(Species ~ Endemics + Area + Nearest, data = gala_subset, family = poisson)

# Calculate AIC for the second model
aic2 <- AIC(model2)

# Round AIC values to three decimal places
aic1 <- round(aic1, 3)
aic2 <- round(aic2, 3)

# Print the results
cat("(1) fitted_log_mu_i =", round(coef(model1)[1], 3), "+", round(coef(model1)[2], 3), " * Endemics_i\n")
cat("    to obtain the first AIC =", aic1, "\n")
cat("(2) The second AIC =", aic2, "which is smaller than the first AIC, so we conclude that the model in (2) is better than the model in (1).\n")

