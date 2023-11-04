# Load the required library and load the dataset
library(faraway)
data(motorins)

# Subset the data to the first 45 rows
motorins_subset <- motorins[1:45, ]
ncol(motorins_subset)
# Fit a gamma GLM with mu_i as the mean of perd_i by Bonus_i
model1 <- glm(perd ~ Bonus, data = motorins_subset, family = Gamma(link = "log"))

# Calculate AIC for the first model
aic1 <- AIC(model1)

# Fit a gamma GLM with mu_i as the mean of perd_i by Bonus_i, Claims_i, and Insured_i
model2 <- glm(perd ~ Bonus + Claims + Insured, data = motorins_subset, family = Gamma(link = "log"))

# Calculate AIC for the second model
aic2 <- AIC(model2)

# Perform the chi-square test
deviance_diff <- anova(model1, model2, test = "Chisq")$Deviance[2]
p_value <- 1 - pchisq(deviance_diff, df = 3)  # Degrees of freedom = 3
p_value
# Round coefficients, AIC values, and p-value to specified decimal places
coef_model1 <- coef(model1)
coef_model2 <- coef(model2)

fitted_log_mu_1 <- round(coef_model1[1], 2)
fitted_log_mu_2 <- round(coef_model2[1], 2)
coef_bonus_1 <- round(coef_model1[2], 2)
coef_bonus_2 <- round(coef_model2[2], 2)
coef_claims_2 <- round(coef_model2[3], 2)
coef_insured_2 <- round(coef_model2[4], 3)

aic1 <- round(aic1, 2)
aic2 <- round(aic2, 2)
deviance_diff <- round(deviance_diff, 2)
p_value <- round(p_value, 2)
p_value

# Print the results
cat("(1) fitted_log_mu_i =", fitted_log_mu_1, "+", coef_bonus_1, " * Bonus_i\n")
cat("    with AIC =", aic1, "\n")
cat("(2) fitted_log_mu_i =", fitted_log_mu_2, "+", coef_bonus_2, " * Bonus_i +", coef_claims_2, "E-04 * Claims_i +", coef_insured_2, "E-05 * Insured_i\n")
cat("    with AIC =", aic2, "\n")
cat("(3) Chi-square test statistic (deviance) =", deviance_diff, "and p-value =", p_value, "\n")
cat("    which is not smaller than 0.05, so we cannot reject H0. We conclude that model in (1) is better than model in (2).\n")
