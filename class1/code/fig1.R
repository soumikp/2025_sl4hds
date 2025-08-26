rm(list = ls())

pacman::p_load(tidyverse, patchwork, here)

### regression example
reg_data <- read_csv(file.path(here(), "class1", "data", "neonatal_brain_volumes.csv"))

plot1 <- reg_data |>
  ggplot(aes(y = `brain volume`, x = GA)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = TRUE, fill = "blue") +
  labs(x = "", y = "Outcome: Brain volume (in mL)",
       title = "Linear model") +
  theme_big_simple()

plot2 <- reg_data |>
  ggplot(aes(y = `brain volume`, x = GA)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = TRUE, fill = "red", span = 0.2) +
  labs(x = "Predictor: Gestational age (in weeks)", y = "",
       title = "LOESS (LOcally Estimated Scatterplot Smoothing)") +
  theme_big_simple()

plot3 <- reg_data |>
  ggplot(aes(y = `brain volume`, x = GA)) +
  geom_point() +
  geom_smooth(method = "gam", color = "forestgreen", se = TRUE, fill = "forestgreen") +
  labs(x = "", y = "",
       title = "GAM (Generalised Additive Model)") +
  theme_big_simple()

fig1 <- plot1 + plot2 + plot3

ggsave(file.path(here(), "class1", "figures", "fig1.pdf"),
       fig1,
       device = pdf,
       width = 24,
       height = 6,
       units = "in")
