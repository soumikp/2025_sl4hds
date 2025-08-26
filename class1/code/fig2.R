rm(list = ls())

pacman::p_load(tidyverse, patchwork, here, caret)

### classification example

class_data <- read_csv(file.path(here(), "class1", "data", "heart_failure_data.csv"))

# making labels
class_data$HF <- factor(class_data$HF, levels = c(0, 1), labels = c("No", "Yes"))

# using caret for explanation, will use in class later.
trainIndex <- createDataPartition(class_data$HF, p = 1, list = FALSE)
train_data <- class_data[trainIndex, ]

train_control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = TRUE,
  classProbs = TRUE
)

svm_linear <- train(
  HF ~ .,
  data = train_data,
  method = "svmLinear",
  trControl = train_control,
  tuneGrid = data.frame(C = c(0.1, 1, 10, 100)),
  preProcess = c("center", "scale")  # Important for SVM
)



# Get feature ranges
x_range <- range(train_data$GLS)
y_range <- range(train_data$EF)

# Create grid
grid <- expand.grid(
  GLS = seq(x_range[1], x_range[2], length.out = 250),
  EF = seq(y_range[1], y_range[2], length.out = 250)
)

# Predict on grid
grid$prediction <- predict(svm_linear, grid)
grid$probability <- predict(svm_linear, grid, type = "prob")[, 1]

# Create plot
fig2_top <- ggplot() +
  # decision regions
  #geom_point(data = grid, aes(x = EF, y = GLS, color = prediction), size = 0.5, show.legend = FALSE) +
  #scale_color_manual(values = friendly_pal("wong_eight")) +
  new_scale_color() +
  geom_point(data = class_data, aes(x = EF, y = GLS, color = HF),  size = 3) +
  labs(x = "Biomarker 1: Ejection Fraction", y = "Biomarker 2: Global Longitudinal Strain",
       color = "Heart failure", title = "Classification") +
  theme_big_simple() +
  scale_color_manual(values = friendly_pal("ito_seven")) +
  theme(legend.position = "none")

fig2_bottom <- ggplot() +
  # decision regions
  geom_point(data = grid, aes(x = EF, y = GLS, color = prediction), size = 0.5, show.legend = FALSE) +
  scale_color_manual(values = friendly_pal("wong_eight")) +
  new_scale_color() +
  geom_point(data = class_data, aes(x = EF, y = GLS, color = HF),  size = 3) +
  labs(x = "Biomarker 1: Ejection Fraction", y = "Biomarker 2: Global Longitudinal Strain",
       color = "Heart failure", title = "") +
  theme_big_simple() +
  scale_color_manual(values = friendly_pal("ito_seven")) +
  theme(legend.position = "none")

fig2 <- (fig2_top/fig2_bottom &
  theme(legend.position = "bottom")) +
  plot_layout(guides = "collect",
              axis_titles = "collect")

ggsave(file.path(here(), "class1", "figures", "fig2.pdf"),
       fig2,
       device = pdf,
       width = 24,
       height = 16,
       units = "in")
