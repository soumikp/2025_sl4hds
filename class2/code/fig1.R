rm(list = ls())
source(file.path(here::here(), "class2", "code", "helper.R"))

pacman::p_load(here, tidyverse, patchwork, latex2exp, ggpubfigs)

set.seed(314159265)
n <- 250
x <- runif(n, -2, 1)
y <- 2*x*x + 4*x + 5
y_e <- y + rnorm(n, 0, 1)

fig1 <- ((data.frame(x, y, y_e) |>
            ggplot(aes(x = x, y = y_e)) +
            geom_point(color = friendly_pal("vibrant_seven")[1]) +
            geom_line(aes(x = x, y = y), color = friendly_pal("vibrant_seven")[5], size = 2) +
            theme_big_simple() +
            labs(x = "", y = "", title = TeX("True model: $Y = 2X^2 + 4X + 5$")) +
            theme(plot.title = element_text(hjust = 0.5))) +
           (data.frame(x, y, y_e) |>
              ggplot(aes(x = x, y = y_e)) +
              geom_point(color = friendly_pal("vibrant_seven")[1]) +
              geom_smooth(aes(x = x, y = y_e), method="lm", se = FALSE,
                          color = friendly_pal("vibrant_seven")[4], size = 2) +
              theme_big_simple() +
              labs(x = "", y = "", title = "Fitting a linear model (p = 1)") +
              theme(plot.title = element_text(hjust = 0.5))))/
  ((data.frame(x, y, y_e) |>
      ggplot(aes(x = x, y = y_e)) +
      geom_point(color = friendly_pal("vibrant_seven")[1]) +
      geom_smooth(aes(x = x, y = y_e), method="lm", se = FALSE,
                  formula=y_e ~ poly(x, 2, raw=TRUE),
                  color = friendly_pal("vibrant_seven")[4], size = 2) +
      theme_big_simple() +
      labs(x = "", y = "", title = "Fitting a quadratic model (p = 2)") +
      theme(plot.title = element_text(hjust = 0.5))) +
     (data.frame(x, y, y_e) |>
        ggplot(aes(x = x, y = y_e)) +
        geom_point(color = friendly_pal("vibrant_seven")[1]) +
        geom_smooth(aes(x = x, y = y_e), method="lm", se = FALSE,
                    formula=y_e ~ poly(x, 20, raw=TRUE),
                    color = friendly_pal("vibrant_seven")[4], size = 2) +
        theme_big_simple() +
        labs(x = "", y = "", title = "Fitting a polynomial model (p = 20)") +
        theme(plot.title = element_text(hjust = 0.5))))


ggsave(file.path(here(), "class2", "figures", "fig1.pdf"),
       fig1,
       device = pdf,
       width = 24,
       height = 16,
       units = "in")

