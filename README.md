# Statistical Modeling & Econometrics in R

This repository contains a collection of advanced statistical modeling and data analysis scripts developed in `R` during my coursework in Regression Analysis and Statistics. The projects demonstrate proficiency in hypothesis testing, regression analysis, econometric diagnostics, and simulation methods.

##  Portfolio Overview

### 1. Monte Carlo Simulation & Likelihood Ratio Test (`1_monte_carlo_hypothesis_testing.R`)
* **Description:** Developed an asymptotic Likelihood Ratio test to distinguish between two Beta distributions. Analytically derived the Fisher Information and calculated the minimax sample size.
* **Key Techniques:** Monte Carlo simulations (10,000 iterations) to refine asymptotic decision thresholds and calculate empirical statistical power.

### 2. Feature Engineering & Multicollinearity Resolution (`2_feature_engineering_regression.R`)
* **Description:** Diagnosed a failed multiple regression model that suffered from severe multicollinearity between predictors.
* **Key Techniques:** Scatterplot matrix diagnostics, Feature Engineering (created a learning efficiency metric), and robust model fitting. The engineered feature successfully transformed a statistically insignificant model into a highly predictive one.

### 3. Non-linear Regression & Structural Break Testing (`3_chow_test_econometrics.R`)
* **Description:** Addressed severe heteroscedasticity and right-skewness in survey data by transitioning to log-log econometric models.
* **Key Techniques:** Logarithmic transformations, residual diagnostics (Q-Q plots), and **Chow Test** application to detect structural breaks across demographic groups.

##  Technologies
* **Language:** `R`
* **Libraries:** `car`, `stats`, base R graphics
