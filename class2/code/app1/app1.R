# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- User Interface (UI) ---
ui <- fluidPage(
  # Use a modern theme
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

  # App title
  titlePanel("Polynomial Regression: Underfitting and Overfitting"),

  # Layout with a sidebar and main area
  sidebarLayout(
    # Sidebar for user inputs
    sidebarPanel(
      h4("1. Data Generation"),
      # UPDATED: Dropdown for true function
      selectInput("true_func", "True Generating Function:",
                  choices = c("Linear",
                              "Cubic",
                              "Polynomial (Degree 7)")),

      # UPDATED: Slider for sample size
      sliderInput("n_points", "Number of Data Points:",
                  min = 50, max = 250, value = 100, step = 50),

      # Slider for the amount of noise in the data
      sliderInput("noise_sd", "Noise Level (Standard Deviation):",
                  min = 0, max = 2, value = 0.5, step = 0.1),

      # Action button to generate a new sample
      actionButton("resample_btn", "Generate New Data", class = "btn-primary w-100"),

      hr(),
      h4("2. Model Complexity"),
      # Slider for the degree of the polynomial
      sliderInput("poly_degree", "Degree of Polynomial:",
                  min = 1, max = 10, value = 1, step = 1, # UPDATED: Max degree is now 10
                  animate = animationOptions(interval = 500, loop = FALSE))
    ),

    # Main panel for displaying outputs
    mainPanel(
      fluidRow(
        # Plot showing the data and the fitted model
        column(7,
               h4("Fitted Polynomial Model"),
               plotOutput("main_plot", height = "450px")
        ),
        # Plot showing the training vs. test error
        column(5,
               h4("Bias-Variance Tradeoff"),
               plotOutput("error_plot", height = "450px")
        )
      ),
      hr(),
      fluidRow(
        # Column for current model performance
        column(12, # Changed from 6 to 12 to take full width
               h4("Current Model Performance"),
               # Text output for MSE values
               verbatimTextOutput("mse_text")
        )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output) {

  # Reactive expression to generate and split data
  # Re-runs when the "Generate New Data" button is pressed
  dataset <- eventReactive(input$resample_btn, {
    n <- input$n_points
    noise <- input$noise_sd
    func_type <- input$true_func

    # Generate x values, scaled for better polynomial behavior
    x <- seq(-5, 5, length.out = n)

    # UPDATED: Generate true y values based on the selected function
    true_y <- switch(func_type,
                     "Linear"                = 2 * x + 3,
                     "Cubic"                 = 0.5 * (x^3) - 4 * x,
                     "Polynomial (Degree 7)" = 0.01 * (x-4) * (x-2) * x * (x+1) * (x+3) * (x+5) * (x+2)
    )

    # Add random noise to create the observed y values
    observed_y <- true_y + rnorm(n, mean = 0, sd = noise)

    # Create a data frame
    df <- data.frame(x = x, y = observed_y, true_y = true_y)

    # --- Data Splitting Note ---
    # For this educational app, we split into two sets: training and testing.
    # The "testing" set here is used to visually demonstrate the bias-variance
    # tradeoff and find the best model complexity (hyperparameter tuning).
    # In a rigorous, real-world workflow, this would be called a "validation" set.

    train_indices <- sample(1:n, size = round(0.7 * n))
    train_data <- df[train_indices, ]
    test_data <- df[-train_indices, ]

    list(train = train_data, test = test_data)
  }, ignoreNULL = FALSE)

  # Reactive expression to fit the polynomial model
  fit_model <- reactive({
    req(dataset())
    train_data <- dataset()$train
    lm(y ~ poly(x, degree = input$poly_degree, raw = TRUE), data = train_data)
  })

  # Reactive expression to calculate Mean Squared Error (MSE) for all degrees
  error_data <- reactive({
    req(dataset())
    train_data <- dataset()$train
    test_data <- dataset()$test
    max_degree <- 10 # UPDATED: Max degree for calculation is now 10

    sapply(1:max_degree, function(degree) {
      model <- lm(y ~ poly(x, degree = degree, raw = TRUE), data = train_data)
      train_pred <- predict(model, newdata = train_data)
      train_mse <- mean((train_data$y - train_pred)^2)
      test_pred <- predict(model, newdata = test_data)
      test_mse <- mean((test_data$y - test_pred)^2)
      c(degree = degree, train_mse = train_mse, test_mse = test_mse)
    }) %>%
      t() %>%
      as.data.frame() %>%
      pivot_longer(cols = c(train_mse, test_mse), names_to = "type", values_to = "mse")
  })

  # --- Outputs ---

  # Render the main plot with data and fitted curve
  output$main_plot <- renderPlot({
    req(dataset(), fit_model())

    train_data <- dataset()$train
    test_data <- dataset()$test
    model <- fit_model()

    x_curve <- seq(min(train_data$x), max(train_data$x), length.out = 200)
    y_curve <- predict(model, newdata = data.frame(x = x_curve))

    # Dynamically set y-axis limits
    full_data <- bind_rows(train_data, test_data)
    y_range <- range(full_data$y, full_data$true_y, na.rm = TRUE)
    y_buffer <- (y_range[2] - y_range[1]) * 0.1
    plot_ylim <- c(y_range[1] - y_buffer, y_range[2] + y_buffer)

    ggplot() +
      geom_line(data = train_data, aes(x = x, y = true_y, color = "True Function"), size = 1.2, linetype = "dashed") +
      geom_point(data = train_data, aes(x = x, y = y, fill = "Training Data"), shape = 21, size = 3, alpha = 0.7) +
      geom_point(data = test_data, aes(x = x, y = y, fill = "Testing Data"), shape = 24, size = 3, alpha = 0.7) +
      geom_line(data = data.frame(x = x_curve, y = y_curve), aes(x = x, y = y, color = "Fitted Model"), size = 1.5) +
      scale_color_manual(name = "Lines", values = c("True Function" = "black", "Fitted Model" = "#dc3545")) +
      scale_fill_manual(name = "Data", values = c("Training Data" = "#007bff", "Testing Data" = "#28a745")) +
      labs(title = paste("Polynomial of Degree", input$poly_degree), x = "Feature (x)", y = "Outcome (y)") +
      coord_cartesian(ylim = plot_ylim) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # Render the error plot (MSE vs. Degree)
  output$error_plot <- renderPlot({
    req(error_data())

    error_df <- error_data()

    # UPDATED: Dynamically set y-axis to zoom in on the error range
    min_mse <- min(error_df$mse, na.rm = TRUE)
    max_mse <- max(error_df$mse, na.rm = TRUE)
    padding <- (max_mse - min_mse) * 0.1 # Add 10% padding

    # Ensure the lower bound is not negative
    plot_ylim_lower <- max(0, min_mse - padding)
    plot_ylim_upper <- max_mse + padding

    ggplot(error_df, aes(x = degree, y = mse, color = type, group = type)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_vline(xintercept = input$poly_degree, linetype = "dotted", color = "black", size = 1) +
      scale_color_manual(name = "Error Type",
                         labels = c("train_mse" = "Training Error", "test_mse" = "Testing Error"),
                         values = c("train_mse" = "#007bff", "test_mse" = "#fd7e14")) +
      scale_x_continuous(breaks = 1:10) + # UPDATED: Axis breaks go to 10
      coord_cartesian(ylim = c(plot_ylim_lower, plot_ylim_upper)) +
      labs(title = "Error vs. Model Complexity", x = "Polynomial Degree", y = "Mean Squared Error (MSE)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # Render the text output for current MSE values
  output$mse_text <- renderPrint({
    req(error_data())

    current_errors <- error_data() %>%
      filter(degree == input$poly_degree) %>%
      pivot_wider(names_from = type, values_from = mse)

    cat(paste("Current Model (Degree", input$poly_degree, "):\n"))
    cat("---------------------------\n")
    cat(paste("Training MSE:", round(current_errors$train_mse, 4), "\n"))
    cat(paste("Testing MSE :", round(current_errors$test_mse, 4), "\n"))
  })

  # REMOVED the best_model_ui output block

}

# Run the application
shinyApp(ui = ui, server = server)
