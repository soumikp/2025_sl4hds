rm(list = ls())
source(file.path(here::here(), "class2", "code", "helper.R"))

pacman::p_load(shiny, ggplot2, dplyr, cluster, DT, shinythemes)

# --- Define UI for the application ---
ui <- fluidPage(
  theme = shinytheme("cosmo"),

  # Application title
  titlePanel("Interactive ML Metric Calculator"),

  # Sidebar with a dropdown to select the metric
  sidebarLayout(
    sidebarPanel(
      h4("Select a Metric"),
      p("Choose a metric from the list below to see a step-by-step calculation and visualization."),
      selectInput("metric", "Metric:",
                  choices = c("RMSE (Root Mean Squared Error)",
                              "Accuracy",
                              "Silhouette Score",
                              "Explained Variance")),
      hr(),
      h4("About"),
      p("This app visualizes how common machine learning metrics are calculated using toy data for educational purposes.")
    ),

    # Main panel to show the outputs for the selected metric
    mainPanel(
      uiOutput("metric_output")
    )
  )
)

# --- Define server logic ---
server <- function(input, output) {

  # Dynamically render UI based on the selected metric
  output$metric_output <- renderUI({
    switch(input$metric,
           "RMSE (Root Mean Squared Error)" = rmse_ui(),
           "Accuracy" = accuracy_ui(),
           "Silhouette Score" = silhouette_ui(),
           "Explained Variance" = explained_variance_ui()
    )
  })

  # --- 1. RMSE UI and Logic ---
  rmse_ui <- function() {
    tagList(
      h3("RMSE: Root Mean Squared Error"),
      p("RMSE measures the average magnitude of the errors between predicted and actual values. It's the square root of the average of squared differences. It is used for ", strong("Regression"), " tasks."),
      h4("Step 1: Toy Data"),
      p("Here are some sample actual and predicted values."),
      DTOutput("rmse_table"),
      h4("Step 2: Formula"),
      withMathJax("$$RMSE = \\sqrt{\\frac{1}{n}\\sum_{i=1}^{n}(predicted_i - actual_i)^2}$$"),
      h4("Step 3: Calculation Breakdown"),
      DTOutput("rmse_calc_table"),
      h4("Step 4: Final Result"),
      uiOutput("rmse_result"),
      h4("Step 5: Visualization"),
      p("The plot below shows the actual vs. predicted values. The red lines represent the error (residuals) for each point, which are the values that get squared in the formula."),
      plotOutput("rmse_plot")
    )
  }

  # Generate reactive data for RMSE
  rmse_data <- reactive({
    set.seed(42)
    data.frame(
      ID = 1:10,
      Actual = round(runif(10, 50, 150), 1),
      Predicted = round(runif(10, 50, 150), 1)
    ) %>%
      mutate(
        Error = Predicted - Actual,
        Squared_Error = Error^2
      )
  })

  output$rmse_table <- renderDT({
    datatable(rmse_data()[, c("ID", "Actual", "Predicted")], options = list(dom = 't', pageLength = 10), rownames = FALSE)
  })

  output$rmse_calc_table <- renderDT({
    datatable(round(rmse_data()[, c("ID", "Error", "Squared_Error")], 2), options = list(dom = 't', pageLength = 10), rownames = FALSE)
  })

  output$rmse_result <- renderUI({
    mse <- mean(rmse_data()$Squared_Error)
    rmse <- sqrt(mse)
    tagList(
      p(paste0("Mean of Squared Errors (MSE) = ", round(mse, 2))),
      p(strong(paste0("RMSE = sqrt(MSE) = ", round(rmse, 2))))
    )
  })

  output$rmse_plot <- renderPlot({
    ggplot(rmse_data(), aes(x = Actual, y = Predicted)) +
      geom_point(color = "blue", size = 4, alpha = 0.7) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
      geom_segment(aes(xend = Actual, yend = Actual), color = "red") +
      labs(title = "Actual vs. Predicted Values", x = "Actual Values", y = "Predicted Values") +
      theme_minimal(base_size = 15) +
      coord_equal()
  })


  # --- 2. Accuracy UI and Logic ---
  accuracy_ui <- function() {
    tagList(
      h3("Accuracy"),
      p("Accuracy is the proportion of correct predictions among the total number of cases evaluated. It is the most common metric for ", strong("Classification"), " tasks, but can be misleading for imbalanced datasets."),
      h4("Step 1: Toy Data"),
      p("Sample actual labels and predicted labels for a binary classification task (0 or 1)."),
      DTOutput("accuracy_table"),
      h4("Step 2: Confusion Matrix"),
      p("The confusion matrix summarizes the performance of the classification algorithm."),
      plotOutput("accuracy_confusion_matrix"),
      h4("Step 3: Formula"),
      withMathJax("$$Accuracy = \\frac{True\\,Positives + True\\,Negatives}{Total\\,Samples}$$"),
      h4("Step 4: Calculation & Result"),
      uiOutput("accuracy_result")
    )
  }

  # Generate reactive data for Accuracy
  accuracy_data <- reactive({
    set.seed(42)
    data.frame(
      ID = 1:20,
      Actual = factor(sample(0:1, 20, replace = TRUE)),
      Predicted = factor(sample(0:1, 20, replace = TRUE, prob = c(0.6, 0.4)))
    )
  })

  output$accuracy_table <- renderDT({
    datatable(accuracy_data(), options = list(dom = 't', pageLength = 20), rownames = FALSE)
  })

  confusion_matrix_data <- reactive({
    cm <- table(Actual = accuracy_data()$Actual, Predicted = accuracy_data()$Predicted)
    data.frame(cm)
  })

  output$accuracy_confusion_matrix <- renderPlot({
    ggplot(data = confusion_matrix_data(), aes(x = Predicted, y = Actual, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), vjust = 1, size = 8, color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label") +
      theme_minimal(base_size = 15) +
      theme(legend.position = "none")
  })

  output$accuracy_result <- renderUI({
    cm <- table(Actual = accuracy_data()$Actual, Predicted = accuracy_data()$Predicted)
    tp <- cm[2, 2]
    tn <- cm[1, 1]
    total <- sum(cm)
    acc <- (tp + tn) / total

    tagList(
      p(paste0("True Positives (Predicted=1, Actual=1): ", tp)),
      p(paste0("True Negatives (Predicted=0, Actual=0): ", tn)),
      p(paste0("Total Samples: ", total)),
      p(strong(paste0("Accuracy = (", tp, " + ", tn, ") / ", total, " = ", round(acc, 3))))
    )
  })

  # --- 3. Silhouette Score UI and Logic ---
  silhouette_ui <- function() {
    tagList(
      h3("Silhouette Score"),
      p("The Silhouette Score measures how similar an object is to its own cluster (cohesion) compared to other clusters (separation). It is used for evaluating ", strong("Clustering"), " performance. The score ranges from -1 to +1, where a high value indicates that the object is well matched to its own cluster and poorly matched to neighboring clusters."),
      h4("Step 1: Toy Data & Clustering"),
      p("Here is some 2D data, which we group into 2 clusters using K-Means."),
      plotOutput("silhouette_cluster_plot"),
      h4("Step 2: Formula"),
      p("For a single sample 'i', the silhouette score s(i) is:"),
      withMathJax("$$s(i) = \\frac{b(i) - a(i)}{max(a(i), b(i))}$$"),
      p(HTML("<ul><li><b>a(i)</b>: The average distance from 'i' to the other points in the same cluster.</li><li><b>b(i)</b>: The average distance from 'i' to all points in the nearest cluster.</li></ul>")),
      p("The overall Silhouette Score is the average of s(i) for all samples."),
      h4("Step 3: Visualization & Result"),
      p("The silhouette plot below displays the score for each sample, grouped by cluster."),
      plotOutput("silhouette_plot"),
      uiOutput("silhouette_result")
    )
  }

  # Generate reactive data for Silhouette Score
  silhouette_data <- reactive({
    set.seed(42)
    data.frame(
      x = c(rnorm(25, 3, 1), rnorm(25, 9, 1)),
      y = c(rnorm(25, 3, 1), rnorm(25, 9, 1))
    )
  })

  kmeans_result <- reactive({
    kmeans(silhouette_data(), centers = 2, nstart = 25)
  })

  silhouette_values <- reactive({
    dist_matrix <- dist(silhouette_data())
    silhouette(kmeans_result()$cluster, dist_matrix)
  })

  output$silhouette_cluster_plot <- renderPlot({
    data <- silhouette_data()
    data$cluster <- as.factor(kmeans_result()$cluster)
    ggplot(data, aes(x = x, y = y, color = cluster)) +
      geom_point(size = 4, alpha = 0.8) +
      labs(title = "Clustered Data Points", x = "Feature 1", y = "Feature 2") +
      theme_minimal(base_size = 15)
  })

  output$silhouette_plot <- renderPlot({
    plot(silhouette_values(), col = 1:2, border = NA, main = "Silhouette Plot")
  })

  output$silhouette_result <- renderUI({
    avg_score <- mean(silhouette_values()[, "sil_width"])
    strong(paste0("Average Silhouette Score = ", round(avg_score, 3)))
  })

  # --- 4. Explained Variance UI and Logic ---
  explained_variance_ui <- function() {
    tagList(
      h3("Explained Variance"),
      p("In dimensionality reduction techniques like Principal Component Analysis (PCA), Explained Variance refers to the proportion of the dataset's total variance that is accounted for by each principal component. It helps in deciding how many components to keep."),
      h4("Step 1: Toy Data"),
      p("We create a 2D dataset where the two features are correlated."),
      plotOutput("ev_data_plot"),
      h4("Step 2: PCA and Variance Calculation"),
      p("PCA finds new axes (principal components) that maximize variance. The table below shows the variance captured by each component."),
      DTOutput("ev_table"),
      h4("Step 3: Scree Plot Visualization"),
      p("A 'scree plot' is used to visualize the explained variance for each component. It helps identify the 'elbow' point where adding more components doesn't add much information."),
      plotOutput("ev_scree_plot"),
      uiOutput("ev_result")
    )
  }

  # Generate reactive data for Explained Variance
  ev_data <- reactive({
    set.seed(42)
    cov_matrix <- matrix(c(5, 4, 4, 5), nrow = 2)
    data <- as.data.frame(MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = cov_matrix))
    names(data) <- c("Feature_1", "Feature_2")
    data
  })

  pca_result <- reactive({
    prcomp(ev_data(), scale. = TRUE)
  })

  ev_df <- reactive({
    vars <- pca_result()$sdev^2
    props <- vars / sum(vars)
    data.frame(
      Component = paste0("PC", 1:length(vars)),
      Variance = vars,
      Proportion_of_Variance = props,
      Cumulative_Proportion = cumsum(props)
    )
  })

  output$ev_data_plot <- renderPlot({
    ggplot(ev_data(), aes(x = Feature_1, y = Feature_2)) +
      geom_point(alpha = 0.7, color = "darkgreen") +
      labs(title = "Original Correlated Data", x = "Feature 1", y = "Feature 2") +
      theme_minimal(base_size = 15)
  })

  output$ev_table <- renderDT({
    # FIX: Only round numeric columns to avoid the error
    df_to_show <- ev_df() %>%
      mutate(across(where(is.numeric), ~ round(., 3)))
    datatable(df_to_show, options = list(dom = 't'), rownames = FALSE)
  })

  output$ev_scree_plot <- renderPlot({
    # Ensure the Component column is treated as a factor to maintain order in the plot
    df_for_plot <- ev_df()
    df_for_plot$Component <- factor(df_for_plot$Component, levels = df_for_plot$Component)

    ggplot(df_for_plot, aes(x = Component, y = Proportion_of_Variance)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = paste0(round(Proportion_of_Variance * 100), "%")), vjust = -0.5) +
      labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained") +
      theme_minimal(base_size = 15) +
      scale_y_continuous(labels = scales::percent)
  })

  output$ev_result <- renderUI({
    pc1_var <- ev_df()$Proportion_of_Variance[1]
    p(strong(paste0("In this case, the first principal component explains ", round(pc1_var * 100), "% of the total variance in the data.")))
  })

}

# --- Run the application ---
shinyApp(ui = ui, server = server)

