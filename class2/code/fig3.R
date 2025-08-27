# Step 1: Load necessary libraries
# install.packages(c("ggplot2", "dplyr", "tidyr")) # Run this if you don't have them
library(ggplot2)
library(dplyr)
library(tidyr)

# Step 2: Use 'mtcars' and fit the initial model
# We standardize numeric predictors to make coefficients more comparable.
mtcars_std <- mtcars %>%
  mutate(across(c(wt, hp), scale))

# Fit the original model to get the point estimates
model_original <- lm(mpg ~ wt + hp + am, data = mtcars_std)
original_estimates <- coef(model_original)

# Step 3: Run the bootstrap procedure (same as before)
set.seed(123) # for reproducibility
n_boot <- 2000 # Number of bootstrap replicates

boot_coefs <- matrix(NA, nrow = n_boot, ncol = length(original_estimates))
colnames(boot_coefs) <- names(original_estimates)

for (i in 1:n_boot) {
  boot_sample <- mtcars_std[sample(1:nrow(mtcars_std), replace = TRUE), ]
  model_boot <- lm(mpg ~ wt + hp + am, data = boot_sample)
  boot_coefs[i, ] <- coef(model_boot)
}

# Step 4: Calculate CIs from BOTH methods

# 4a. Bootstrap-based CI (from percentiles)
boot_ci <- data.frame(
  variable = names(original_estimates),
  estimate = original_estimates,
  ci_lower = apply(boot_coefs, 2, quantile, probs = 0.025, na.rm = TRUE),
  ci_upper = apply(boot_coefs, 2, quantile, probs = 0.975, na.rm = TRUE),
  method = "Bootstrap"
)

# 4b. OLS theory-based CI (using confint function)
ols_ci_matrix <- confint(model_original)
ols_ci <- data.frame(
  variable = rownames(ols_ci_matrix),
  estimate = original_estimates,
  ci_lower = ols_ci_matrix[, 1],
  ci_upper = ols_ci_matrix[, 2],
  method = "OLS Theory"
)

# Step 5: Combine results and prepare for plotting
# We bind the two data frames together and filter out the intercept.
plot_data_combined <- bind_rows(boot_ci, ols_ci) %>%
  filter(variable != "(Intercept)")

# Step 6: Create the comparative forest plot
# We use position_dodge to place the CIs for each method side-by-side.
fig3 <- ggplot(plot_data_combined, aes(x = estimate, y = reorder(variable, estimate), color = method)) +
  # Add the CI error bars, dodged by method
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 height = 0.5, linewidth = 0.8, position = position_dodge(width = 0.6)) +
  # Add the point estimates, also dodged
  geom_point(size = 4, shape = 23, aes(fill = method),
             position = position_dodge(width = 0.6)) +
  # Add a vertical line at zero (the null effect)
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # Custom colors and labels
  scale_color_manual(values = c("Bootstrap" = "red", "OLS Theory" = "black")) +
  scale_fill_manual(values = c("Bootstrap" = "red", "OLS Theory" = "black")) +
  labs(
    title = "Forest Plot: Bootstrap vs. OLS Theory Confidence Intervals",
    subtitle = "Comparing 95% CIs for each model coefficient",
    x = "Coefficient Estimate (Standardized)",
    y = "Model Term",
    color = "CI Method",
    fill = "CI Method"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

ggsave(file.path(here(), "class2", "figures", "fig3.pdf"),
       fig3,
       device = pdf,
       width = 16,
       height = 8,
       units = "in")
