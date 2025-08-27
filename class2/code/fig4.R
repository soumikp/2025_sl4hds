# Load necessary libraries
library(dplyr)
library(ggplot2)
library(ggridges)  # For raincloud plots
library(patchwork) # For combining plots

# Load the dataset and filter for two groups
data(chickwts)
chick_subset <- chickwts %>%
  filter(feed %in% c("casein", "meatmeal")) %>%
  droplevels()

# --- 1. Create the Data Visualization Plot (Corrected) ---
# We map the continuous variable to x and discrete to y consistently.
plot1 <- ggplot(chick_subset, aes(x = weight, y = feed, fill = feed)) +
  # Add the density ridge layer
  geom_density_ridges(
    alpha = 0.8,
    scale = 0.9, # Adjust vertical scaling
    quantile_lines = TRUE, # Adds lines for quantiles
    quantiles = 2 # Specifically, adds a line for the median
  ) +
  # Add jittered raw data points
  geom_point(
    aes(color = feed),
    shape = "|",
    size = 4,
    alpha = 0.5,
    position = position_nudge(y = -0.2)
  ) +
  labs(
    title = "A) Observed Chick Weights by Feed Type",
    x = "Weight (grams)",
    y = "Feed Type"
  ) +
  scale_fill_discrete(guide = "none") +  # Hide legends
  scale_color_discrete(guide = "none") +
  theme_minimal(base_size = 12)

# --- 2. Implement the Permutation Test (No changes here) ---
observed_diff <- diff(tapply(chick_subset$weight, chick_subset$feed, mean))
n_permutations <- 10000
permutation_diffs <- numeric(n_permutations)

set.seed(123) # for reproducibility
for (i in 1:n_permutations) {
  shuffled_feed <- sample(chick_subset$feed)
  permutation_diffs[i] <- diff(tapply(chick_subset$weight, shuffled_feed, mean))
}

# --- 3. Create the Null Distribution Plot (No changes here) ---
perm_df <- data.frame(diffs = permutation_diffs)

plot2 <- ggplot(perm_df, aes(x = diffs)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.8) +
  geom_vline(xintercept = observed_diff, color = "red", linetype = "dashed", linewidth = 1) +
  annotate(
    "text", x = observed_diff, y = Inf,
    label = paste("Observed Diff =", round(observed_diff, 1)),
    hjust = -0.1, vjust = 1.5, color = "red", size = 4
  ) +
  labs(
    title = "B) Null Distribution from Permutation",
    x = "Difference in Means (Permuted)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12)

# --- 4. Combine the plots using patchwork ---
fig4 <- plot1 + plot2

ggsave(file.path(here(), "class2", "figures", "fig4.pdf"),
       fig4,
       device = pdf,
       width = 16,
       height = 8,
       units = "in")
