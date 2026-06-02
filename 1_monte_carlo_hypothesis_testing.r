# ==============================================================================
# Monte Carlo Simulation & Likelihood Ratio Test for Beta Distribution
# ==============================================================================

# 1. Define initial parameters
n <- 50               # Sample size
alpha0 <- 4           # Alpha parameter for Null Hypothesis (H0)
alpha1 <- 5           # Alpha parameter for Alternative Hypothesis (H1)
beta_param <- 3       # Beta parameter (fixed for both)
alpha_sig <- 0.05     # Significance level

# 2. Calculate Fisher Information I(alpha0)
# trigamma(x) computes the second derivative of the logarithm of the gamma function
I_0 <- trigamma(alpha0) - trigamma(alpha0 + beta_param)

# 3. Asymptotic distributions parameters
v <- sqrt(n) * (alpha1 - alpha0) # Approach parameter
mu_0 <- -(v^2 / 2) * I_0         # Mean under H0
sigma <- sqrt(v^2 * I_0)         # Standard deviation
mu_1 <- (v^2 / 2) * I_0          # Mean under H1

# Asymptotic threshold (Right-tailed critical region)
C_asymp <- qnorm(1 - alpha_sig, mean = mu_0, sd = sigma)

# Asymptotic power
beta_asymp <- pnorm(C_asymp, mean = mu_1, sd = sigma)
power_asymp <- 1 - beta_asymp

cat("--- Asymptotic Results ---\n")
cat("Fisher Information I(alpha0):", round(I_0, 5), "\n")
cat("Asymptotic Threshold C:", round(C_asymp, 4), "\n")
cat("Asymptotic Power:", round(power_asymp, 4), "\n\n")

# 4. Monte Carlo Simulation to refine theoretical thresholds
set.seed(42) # For reproducibility
N_sim <- 10000

# Function to calculate Log-Likelihood Ratio
calc_lr <- function(x) {
  sum(dbeta(x, alpha1, beta_param, log = TRUE)) - 
  sum(dbeta(x, alpha0, beta_param, log = TRUE))
}

# Simulate samples assuming H0 is true
lr_H0_sim <- replicate(N_sim, {
  x <- rbeta(n, alpha0, beta_param)
  calc_lr(x)
})

# Refined threshold via Monte Carlo empirical quantile
C_sim <- quantile(lr_H0_sim, 1 - alpha_sig)

# Simulate samples assuming H1 is true to find actual power
lr_H1_sim <- replicate(N_sim, {
  x <- rbeta(n, alpha1, beta_param)
  calc_lr(x)
})

power_sim <- mean(lr_H1_sim > C_sim)

cat("--- Monte Carlo Simulation Results ---\n")
cat("Refined Threshold C (Empirical):", round(C_sim, 4), "\n")
cat("Refined Actual Power:", round(power_sim, 4), "\n\n")

# 5. Minimax Sample Size Calculation
lambda_05 <- qnorm(1 - alpha_sig)
n_minimax <- (lambda_05 + lambda_05)^2 / (I_0 * (alpha1 - alpha0)^2)

cat("--- Minimax Test ---\n")
cat("Required Minimax Sample Size (n):", ceiling(n_minimax), "\n")
