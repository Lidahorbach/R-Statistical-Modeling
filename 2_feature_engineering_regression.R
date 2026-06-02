# ==============================================================================
# Resolving Multicollinearity via Feature Engineering in Multiple Regression
# ==============================================================================
library(car)

# 1. Dataset generation
study_data <- data.frame(
  Hours = c(1.5, 2.0, 3.0, 1.0, 4.0, 2.5, 5.0, 3.5, 2.0, 4.5, 1.0, 3.0),
  Pages = c(20, 30, 45, 15, 60, 40, 70, 50, 25, 65, 10, 42),
  Topics = c(2, 3, 4, 2, 5, 6, 5, 3, 6, 1, 4, 4),
  Score = c(75, 78, 80, 72, 82, 85, 81, 78, 70, 77, 65, 79)
)

# 2. Diagnosing Multicollinearity
# Plotting scatterplot matrix. Notice the almost perfect linear relationship 
# between 'Hours' and 'Pages', indicating severe multicollinearity.
scatterplotMatrix(~Hours+Pages+Topics, data=study_data, diagonal="histogram", smoother=FALSE)

# 3. Model 1: Initial naive model (Fails due to multicollinearity)
model1 <- lm(Topics ~ Hours + Pages, data = study_data)
print(summary(model1)) # p-value ~ 0.95, model is completely insignificant

# 4. Feature Engineering: Creating a 'Reading Speed' metric (Efficiency)
# Instead of raw volume, we measure learning efficiency.
study_data$ReadingSpeed <- study_data$Pages / study_data$Hours

# 5. Model 2: Refined Regression Model
model2 <- lm(Score ~ ReadingSpeed, data = study_data)
print(summary(model2)) # p-value < 0.001, Adjusted R-squared ~ 0.67. Highly significant.

# 6. Residual Diagnostics for Model 2
par(mfrow=c(1,2))

# Q-Q Plot to check for normality of residuals
qqnorm(model2$residuals, main="Normal Q-Q Plot of Residuals")
qqline(model2$residuals, col="red")

# Residuals vs Fitted values to check for homoscedasticity
plot(model2$fitted.values, model2$residuals, 
     main="Residuals vs Fitted",
     xlab="Fitted Predictions", ylab="Residuals")
abline(h=0, col="red")

par(mfrow=c(1,1))
