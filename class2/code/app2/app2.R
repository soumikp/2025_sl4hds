# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define a function to calculate the chosen statistic
calculate_statistic <- function(data, stat_name) {
  switch(stat_name,
         "Mean" = mean(data, na.rm = TRUE),
         "Median" = median(data, na.rm = TRUE),
         "Standard Deviation" = sd(data, na.rm = TRUE),
         "Variance" = var(data, na.rm = TRUE),
         "25th Percentile" = quantile(data, 0.25, na.rm = TRUE),
         "75th Percentile" = quantile(data, 0.75, na.rm = TRUE)
  )
}

# --- User Interface (UI) ---
ui <- fluidPage(
  # Use a theme for a cleaner look
  theme = bslib::bs_theme(version = 4, bootswatch = "minty"),

  # App title
  titlePanel("Interactive Bootstrap Method Demonstration"),

  # Layout with a sidebar and main area
  sidebarLayout(
    # Sidebar for user inputs
    sidebarPanel(
      h4("1. Data Generation"),
      # Dropdown to select the data generating distribution
      selectInput("dist", "Generating Distribution:",
                  choices = c("Normal", "Exponential", "Gamma", "Cauchy")),

      # Conditional UI for Normal distribution parameters
      conditionalPanel(
        condition = "input.dist == 'Normal'",
        numericInput("norm_mean", "Mean", value = 0),
        numericInput("norm_sd", "Standard Deviation", value = 1, min = 0.1)
      ),

      # Conditional UI for Exponential distribution parameters
      conditionalPanel(
        condition = "input.dist == 'Exponential'",
        numericInput("exp_rate", "Rate", value = 1, min = 0.1)
      ),

      # Conditional UI for Gamma distribution parameters
      conditionalPanel(
        condition = "input.dist == 'Gamma'",
        numericInput("gamma_shape", "Shape", value = 2, min = 0.1),
        numericInput("gamma_rate", "Rate", value = 1, min = 0.1)
      ),

      # Conditional UI for Cauchy distribution parameters
      conditionalPanel(
        condition = "input.dist == 'Cauchy'",
        numericInput("cauchy_location", "Location", value = 0),
        numericInput("cauchy_scale", "Scale", value = 1, min = 0.1)
      ),

      hr(),
      h4("2. Sampling Parameters"),
      # Slider for original sample size
      sliderInput("n_sample", "Original Sample Size (n):",
                  min = 10, max = 500, value = 50, step = 10),

      # Slider for number of bootstrap samples
      sliderInput("n_bootstrap", "Number of Bootstrap Samples (B):",
                  min = 100, max = 10000, value = 1000, step = 100),

      hr(),
      h4("3. Statistic to Analyze"),
      # Dropdown to select the summary statistic
      selectInput("statistic", "Summary Statistic:",
                  choices = c("Mean", "Median", "Standard Deviation", "Variance", "25th Percentile", "75th Percentile")),

      hr(),
      # Action button to generate a new sample
      actionButton("resample_btn", "Generate New Original Sample", class = "btn-primary w-100")
    ),

    # Main panel for displaying outputs
    mainPanel(
      fluidRow(
        # Plot for the original sample distribution
        column(6,
               h4("Original Sample Distribution"),
               plotOutput("sample_plot")
        ),
        # Plot for the bootstrap distribution
        column(6,
               h4("Bootstrap Distribution of the Statistic"),
               plotOutput("bootstrap_plot")
        )
      ),
      hr(),
      fluidRow(
        # Column for Bootstrap Summary
        column(6,
               h4("Bootstrap Summary"),
               # Text output for summary statistics
               verbatimTextOutput("summary_text")
        ),
        # Column for Normality Test
        column(6,
               h4("Normality of Bootstrap Distribution"),
               # UI output for the normality test results
               uiOutput("normality_test_ui")
        )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output) {

  # Reactive expression to generate the original sample
  # It re-runs only when the "Generate New Original Sample" button is pressed
  original_sample <- eventReactive(input$resample_btn, {
    n <- input$n_sample
    dist <- input$dist

    # Generate data based on the selected distribution
    switch(dist,
           "Normal" = rnorm(n, input$norm_mean, input$norm_sd),
           "Exponential" = rexp(n, input$exp_rate),
           "Gamma" = rgamma(n, shape = input$gamma_shape, rate = input$gamma_rate),
           "Cauchy" = rcauchy(n, location = input$cauchy_location, scale = input$cauchy_scale)
    )
  }, ignoreNULL = FALSE) # ignoreNULL=FALSE ensures it runs on startup

  # Reactive expression to perform the bootstrap procedure
  bootstrap_dist <- reactive({
    # Ensure original_sample() is available
    req(original_sample())

    sample_data <- original_sample()
    n <- length(sample_data)
    B <- input$n_bootstrap
    stat_name <- input$statistic

    # Use replicate to perform the bootstrap resampling and calculation
    replicate(B, {
      # 1. Resample the original data with replacement
      bootstrap_sample <- sample(sample_data, size = n, replace = TRUE)
      # 2. Calculate the statistic on the new sample
      calculate_statistic(bootstrap_sample, stat_name)
    })
  })

  # --- Outputs ---

  # Render the plot for the original sample
  output$sample_plot <- renderPlot({
    req(original_sample())
    df <- data.frame(value = original_sample())
    ggplot(df, aes(x = value)) +
      geom_histogram(aes(y = ..density..), bins = 20, fill = "#20c997", color = "white", alpha = 0.8) +
      geom_density(color = "#007bff", size = 1.2) +
      labs(title = paste("Histogram of Original Sample (n =", length(original_sample()), ")"),
           x = "Value", y = "Density") +
      theme_minimal(base_size = 14)
  })

  # Render the plot for the bootstrap distribution
  output$bootstrap_plot <- renderPlot({
    req(bootstrap_dist())
    original_stat <- calculate_statistic(original_sample(), input$statistic)
    df <- data.frame(statistic = bootstrap_dist())
    ggplot(df, aes(x = statistic)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#fd7e14", color = "white", alpha = 0.8) +
      geom_density(color = "#dc3545", size = 1.2) +
      geom_vline(xintercept = original_stat, color = "#007bff", linetype = "dashed", size = 1.2) +
      labs(title = paste("Bootstrap Distribution of the", input$statistic),
           subtitle = paste(input$n_bootstrap, "replications"),
           x = "Statistic Value", y = "Density") +
      annotate("text", x = original_stat, y = 0, label = " Original Stat", hjust = -0.1, color = "#007bff", size = 4) +
      theme_minimal(base_size = 14)
  })

  # Render the summary text output
  output$summary_text <- renderPrint({
    req(original_sample(), bootstrap_dist())
    original_stat <- calculate_statistic(original_sample(), input$statistic)
    boot_stats <- bootstrap_dist()
    boot_se <- sd(boot_stats)
    boot_ci <- quantile(boot_stats, probs = c(0.025, 0.975))

    cat("--- Original Sample ---\n")
    cat(paste("Statistic (", input$statistic, "): ", round(original_stat, 4), "\n\n", sep = ""))
    cat("--- Bootstrap Results ---\n")
    cat(paste("Replications:", input$n_bootstrap, "\n"))
    cat(paste("Mean of Statistic:", round(mean(boot_stats), 4), "\n"))
    cat(paste("Standard Error:", round(boot_se, 4), "\n"))
    cat(paste("95% CI: [", round(boot_ci[1], 4), ", ", round(boot_ci[2], 4), "]\n", sep = ""))
  })

  # NEW: Render the UI for the normality test
  output$normality_test_ui <- renderUI({
    req(bootstrap_dist())
    boot_stats <- bootstrap_dist()

    # Shapiro-Wilk test is limited to samples between 3 and 5000
    if (length(boot_stats) > 5000 || length(boot_stats) < 3) {
      return(
        div(class = "alert alert-warning",
            "Shapiro-Wilk test can only be run on 3 to 5000 samples.",
            "Please adjust the number of bootstrap samples.")
      )
    }

    # Perform the test
    shapiro_test <- shapiro.test(boot_stats)

    # Create the output using a styled div
    div(
      p(strong("Shapiro-Wilk Normality Test")),
      p(paste("W statistic:", round(shapiro_test$statistic, 4))),
      p(paste("p-value:", format.pval(shapiro_test$p.value, digits = 4, eps = 0.0001))),
      hr(),
      p(
        em(
          if (shapiro_test$p.value < 0.05) {
            "The p-value is < 0.05, suggesting the bootstrap distribution is not normally distributed."
          } else {
            "The p-value is >= 0.05, so we cannot reject the null hypothesis of normality."
          }
        )
      )
    )
  })

}

# Run the application
shinyApp(ui = ui, server = server)
