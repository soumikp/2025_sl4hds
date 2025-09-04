rm(list = ls())

pacman::p_load(tidyverse, here, ggpubfigs, dplyr, e1071, table1) # For the SVM model

health_data <- read_csv(file.path(here(), "class1", "data", "breast_tumor_measurements.csv"))

# PCA on the numeric features; select only the feature columns (1 through 4)
# crucial to center and scale the data before PCA
pca_result <- prcomp(health_data[, 1:4], center = TRUE, scale. = TRUE)

#table1(~radius + texture + perimeter + area | diagnosis, data = health_data)

# new data frame with the first two Principal Components
# and the original diagnosis labels.
pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  diagnosis = health_data$diagnosis
)

# train a linear SVM classifier to find the boundary between the groups
# train the model on the two principal components.
pca_data$diagnosis <- as.factor(pca_data$diagnosis)
svm_model <- svm(diagnosis ~ PC1 + PC2, data = pca_data, kernel = "linear")

# create a grid of points to visualize the decision boundary
# grid covers the entire plot area.
x_range <- range(pca_data$PC1)
y_range <- range(pca_data$PC2)
grid_points <- expand.grid(PC1 = seq(from = x_range[1], to = x_range[2], length.out = 100),
                           PC2 = seq(from = y_range[1], to = y_range[2], length.out = 100))

# predict the class for each point on the grid
grid_predictions <- predict(svm_model, grid_points)
grid_points$diagnosis <- grid_predictions

#  plot
fig4 <- ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_raster(data = grid_points, aes(fill = diagnosis), alpha = 0.3, interpolate = TRUE, show.legend = FALSE) +
  geom_point(aes(color = diagnosis, shape = diagnosis), size = 3) +
  stat_contour(data = grid_points, aes(z = as.numeric(diagnosis)), breaks = 1.5, color = "black", size = 1) +
  scale_color_manual(values = c("Benign" = "#0072B2", "Malignant" = "#D55E00")) +
  scale_fill_manual(values = c("Benign" = "#0072B2", "Malignant" = "#D55E00")) +
  labs(
    title = "PCA of Breast Tumor Data",
    subtitle = "Separation of Benign and Malignant Tumors with clear boundary",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Diagnosis",
    fill = "Predicted Region",
    shape = "Diagnosis"
  ) +
  theme_big_simple() +
  coord_fixed()


ggsave(file.path(here(), "class1", "figures", "fig4.pdf"),
       fig4,
       device = pdf,
       width = 16,
       height = 8,
       units = "in")
