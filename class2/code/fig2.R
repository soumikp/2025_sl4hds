# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# --- 1. Simulation Setup ---

# Define the true underlying model function
true_model_func <- function(x) {
  x^2 + x - 2
}

# Set simulation parameters
n_points <- 100 # Number of data points in each sample
predictor_range <- seq(-3, 3, length.out = n_points)
noise_sd <- 1.0
degrees_to_fit <- c(1, 2, 20)
n_replicates <- 5

# Create a data frame with the true model values (without noise)
true_df <- data.frame(
  predictor = predictor_range,
  predicted = true_model_func(predictor_range)
)


# --- 2. Run the Simulation ---

# Use a loop to generate data and fit models for each degree and replicate
all_predictions <- lapply(degrees_to_fit, function(deg) {

  replicate_predictions <- lapply(1:n_replicates, function(rep_num) {

    # Step 1 & 2: Generate observed sample with noise
    y_obs <- true_model_func(predictor_range) + rnorm(n_points, mean = 0, sd = noise_sd)
    sample_df <- data.frame(predictor = predictor_range, y_obs = y_obs)

    # Step 3: Fit the polynomial model
    fit <- lm(y_obs ~ poly(predictor, degree = deg, raw = TRUE), data = sample_df)

    # Obtain predicted values for the smooth curve
    predicted_values <- predict(fit, newdata = data.frame(predictor = predictor_range))

    # Return a data frame with results for this specific replicate
    data.frame(
      degree = deg,
      replicate = rep_num,
      predictor = predictor_range,
      predicted = predicted_values
    )
  })

  # Combine results from all replicates for the current degree
  bind_rows(replicate_predictions)
})

# Combine all results into a single data frame
predictions_df <- bind_rows(all_predictions)

# Calculate the average prediction for each degree across all replicates
average_predictions_df <- predictions_df %>%
  group_by(degree, predictor) %>%
  summarise(predicted = mean(predicted), .groups = 'drop')


# --- 3. Generate the Plot ---

fig2 <- ggplot(mapping = aes(x = predictor, y = predicted)) +

  # Plot the individual replicate fits (light blue lines)
  geom_line(data = predictions_df, aes(group = replicate), color = "skyblue") +

  # Plot the true model (thin black line)
  geom_line(data = true_df, color = "black", size = 0.7) +

  # Plot the average over replicates (thick green dashed line)
  geom_line(data = average_predictions_df, color = "darkgreen", size = 1.5, linetype = "dashed", alpha = 0.7) +

  # Create the three panels, one for each degree
  facet_wrap(~ paste("Polynomial model\n(degree =", degree, ")")) +

  # Apply the custom theme from your package
  theme_big_simple() +

  # Add labels
  labs(
    x = "Predictor",
    y = "Predicted"
  ) +
  # Set y-axis limits to match the example image
  coord_cartesian(ylim = range(true_df$predicted, predictions_df$predicted))



ggsave(file.path(here(), "class2", "figures", "fig2.pdf"),
       fig2,
       device = pdf,
       width = 16,
       height = 8,
       units = "in")

