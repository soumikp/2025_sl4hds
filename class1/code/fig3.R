rm(list = ls())

pacman::p_load(ggplot2, caret, e1071, emojifont, ggpubfigs, tidyverse, here)

### clustering example
clust_data <- read_csv(file.path(here(), "class1", "data", "sbp_bmi_measurements.csv"))

# pre-processing object that will center and scale the data.
# calculates the mean and standard deviation for each feature.
preproc_plan <- preProcess(clust_data, method = c("center", "scale"))

# pre-processing plan applied to the dataset.
scaled_health_data <- predict(preproc_plan, clust_data)


# k-means on the SCALED data for a more balanced result.
k_means_result <- kmeans(scaled_health_data, centers = 3, nstart = 25)

# add the cluster assignments back to our ORIGINAL data for plotting.
clust_data$cluster <- as.factor(k_means_result$cluster)

# create the grid in the original data scale
grid <- expand.grid(
  BMI = seq(min(clust_data$BMI) - 1, max(clust_data$BMI) + 1, length.out = 200),
  SBP = seq(min(clust_data$SBP) - 1, max(clust_data$SBP) + 1, length.out = 200)
)

# apply the SAME scaling plan to the grid
scaled_grid <- predict(preproc_plan, grid)

# cluster centers (these are in the scaled space)
scaled_centroids <- k_means_result$centers

# assign each scaled grid point to the nearest scaled centroid
grid$cluster <- as.factor(apply(scaled_grid, 1, function(row) {
  which.min(colSums((t(scaled_centroids) - row)^2))
}))

unscaled_centroids <- as.data.frame(t(t(k_means_result$centers) * preproc_plan$std + preproc_plan$mean))

grid$cluster <- factor(grid$cluster, levels = c(1, 2, 3))
clust_data$cluster <- factor(clust_data$cluster, levels = c(1, 2, 3))

fig3 <- ggplot() +
  geom_raster(data = grid, aes(x = BMI, y = SBP, fill = cluster), alpha = 0.3, show.legend = FALSE) +
  geom_point(data = clust_data, aes(x = BMI, y = SBP, color = cluster), size = 2) +
  geom_point(data = unscaled_centroids, aes(x = BMI, y = SBP), color = "black", shape = 8, size = 5) +
  labs(
    title = "Patient Health Profiles",
    x = "Body Mass Index (BMI)",
    y = "Systolic Blood Pressure (SBP)",
    color = "Patient Cluster"
  ) +
  theme_big_simple() +
  scale_fill_manual(values = friendly_pal("ito_seven")) +
  scale_color_manual(values = friendly_pal("ito_seven"))

ggsave(file.path(here(), "class1", "figures", "fig3.pdf"),
       fig3,
       device = pdf,
       width = 24,
       height = 8,
       units = "in")
