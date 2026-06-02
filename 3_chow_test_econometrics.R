# ==============================================================================
# Econometrics: Log-Log Transformation and Structural Breaks (Chow Test)
# ==============================================================================

# 1. Mocking Data (Normally this is read via read.table("house01.txt"))
# Y = SLIV (Living Area), X = TOTALRES (Total Resources), BATH = factor (1 or 2)
set.seed(123)
N <- 300
TOTALRES <- rlnorm(N, meanlog=8, sdlog=1)
BATH <- sample(c(1, 2), N, replace=TRUE)
# Creating structural difference based on BATH factor
SLIV <- ifelse(BATH == 1, 6.15 * TOTALRES^0.19, 5.67 * TOTALRES^0.22) * exp(rnorm(N, 0, 0.4))
domo <- data.frame(TOTALRES, SLIV, BATH)

# 2. Addressing Heteroscedasticity via Log-Log Transformation
# Initial raw data scatterplot shows severe right-skewness and heteroscedasticity
par(mfrow=c(1,2))
plot(domo$TOTALRES, domo$SLIV, main="Raw Data (Heteroscedastic)", pch=19, col="blue")

domo$L_TOTALRES <- log(domo$TOTALRES)
domo$L_SLIV <- log(domo$SLIV)

# Transformed data scatterplot
plot(domo$L_TOTALRES, domo$L_SLIV, main="Log-Log Data (Stabilized)", pch=19, col="darkgreen")
par(mfrow=c(1,1))

# 3. Chow Test for Structural Breaks
# Restricted Model (ignores BATH grouping)
res_restricted <- lm(L_SLIV ~ L_TOTALRES, data = domo)
RSS_r <- sum(res_restricted$residuals^2)

# Unrestricted Models (separate regressions for each BATH group)
res_bath1 <- lm(L_SLIV ~ L_TOTALRES, data = domo, subset = (BATH == 1))
res_bath2 <- lm(L_SLIV ~ L_TOTALRES, data = domo, subset = (BATH == 2))
RSS_u <- sum(res_bath1$residuals^2) + sum(res_bath2$residuals^2)

# F-Statistic Calculation
df1 <- 2               # Number of restrictions
df2 <- nrow(domo) - 4  # Degrees of freedom for denominator
F_stat <- ((RSS_r - RSS_u) / df1) / (RSS_u / df2)
p_value <- 1 - pf(F_stat, df1, df2)

cat("--- Chow Test Results ---\n")
cat("F-Statistic:", round(F_stat, 4), "\n")
cat("P-value:", signif(p_value, 5), "\n\n")

if(p_value < 0.05) {
  cat("Conclusion: Null hypothesis rejected. Structural break detected based on the BATH factor.\n")
}

# 4. Final Separate Models Summary
cat("\n--- Sub-group 1 (BATH=1) Summary ---\n")
print(summary(res_bath1)$coefficients)

cat("\n--- Sub-group 2 (BATH=2) Summary ---\n")
print(summary(res_bath2)$coefficients)
