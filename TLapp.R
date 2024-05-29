library(shiny)
library(rpart)
library(ggplot2)
library(dplyr)
library(cluster)

# UI Definition
ui <- fluidPage(
  titlePanel("Εφαρμογή Ανάλυσης Δεδομένων"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Επιλογή αρχείου CSV ή Excel:"),
      uiOutput("x_var_selector"),
      uiOutput("y_var_selector")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", tableOutput("data_preview")),
        tabPanel("2D Visualization", plotOutput("scatter_plot")),
        tabPanel("Classification", 
                 selectInput("classification_algorithm", "Επιλογή αλγορίθμου:", 
                             choices = c("Decision Tree", "Logistic Regression")),
                 actionButton("classify_button", "Κατηγοριοποίηση"),
                 verbatimTextOutput("classification_result"),
                 verbatimTextOutput("accuracy_comparison")
        ),
        # Clustering Tab
        tabPanel("Clustering", 
                 selectInput("clustering_algorithm", "Επιλογή αλγορίθμου:",
                             choices = c("K-Means", "Hierarchical Clustering")),
                 actionButton("run_clustering", "Ανάλυση Δεδομένων"),
                 verbatimTextOutput("clustering_result"),
                 verbatimTextOutput("silhouette_comparison")
        ),
        # Info Tab
        tabPanel("Info", 
                 h3("Πληροφορίες για την Εφαρμογή"),
                 p("Η εφαρμογή ανάλυσης δεδομένων έχει σχεδιαστεί για να επιτρέπει στους χρήστες να εκτελούν κατηγοριοποίηση και ομαδοποίηση σε δεδομένα από αρχεία CSV ή Excel. Παρακάτω αναλύονται οι βασικές λειτουργίες της:


  Φόρτωση Δεδομένων:
Οι χρήστες μπορούν να φορτώσουν ένα αρχείο CSV ή Excel.
Μετά τη φόρτωση, εμφανίζεται ένας πίνακας με τα δεδομένα (Data Preview).

  Επιλογή Μεταβλητών:
Οι χρήστες μπορούν να επιλέξουν ποια στήλη των δεδομένων θα χρησιμοποιηθεί ως μεταβλητή X και ποια ως μεταβλητή Y.

  Οπτικοποίηση Δεδομένων:
Δημιουργείται ένα διάγραμμα διασποράς (scatter plot) των επιλεγμένων μεταβλητών για την οπτικοποίηση των δεδομένων (2D Visualization).

  Κατηγοριοποίηση (Classification):
Οι χρήστες μπορούν να επιλέξουν μεταξύ των αλγορίθμων Decision Tree και Logistic Regression.
Πατώντας το κουμπί Κατηγοριοποίηση, εκτελείται ο επιλεγμένος αλγόριθμος στα δεδομένα.
Τα αποτελέσματα της κατηγοριοποίησης και η ακρίβεια του μοντέλου εμφανίζονται στην καρτέλα Classification.
Η εφαρμογή συγκρίνει την ακρίβεια των αλγορίθμων και ενημερώνει ποιος αλγόριθμος έχει την καλύτερη απόδοση.

  Ομαδοποίηση (Clustering):
Οι χρήστες μπορούν να επιλέξουν μεταξύ των αλγορίθμων K-Means και Hierarchical Clustering.
Πατώντας το κουμπί Ανάλυση Δεδομένων, εκτελείται ο επιλεγμένος αλγόριθμος στα δεδομένα.
Τα αποτελέσματα της ομαδοποίησης και το Silhouette score εμφανίζονται στην καρτέλα Clustering.
Η εφαρμογή συγκρίνει τα Silhouette scores των αλγορίθμων και ενημερώνει ποιος αλγόριθμος έχει την καλύτερη απόδοση."),
                 h3("Ομάδα Ανάπτυξης"),
                 p("Μαρία Μπαλασοπούλου - ΑΜ Inf2021150"),
                 p("Θεοδώρα Ματσαρίδου - ΑΜ Inf2021136")
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output) {
  # Data loading from the selected CSV or Excel file
  data <- reactive({
    req(input$file)
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath)
    return(df)
  })
  
  # X variable selection
  output$x_var_selector <- renderUI({
    req(data())
    selectInput("x_var", "Επιλογή μεταβλητής X:", choices = colnames(data()))
  })
  
  # Y variable selection
  output$y_var_selector <- renderUI({
    req(data())
    selectInput("y_var", "Επιλογή μεταβλητής Y:", choices = colnames(data()))
  })
  
  # Preview of all data
  output$data_preview <- renderTable({
    data()
  })
  
  # 2D visualization plot
  output$scatter_plot <- renderPlot({
    req(input$file)
    req(input$x_var)
    req(input$y_var)
    
    x_var <- input$x_var
    y_var <- input$y_var
    
    ggplot(data(), aes(x = !!sym(x_var), y = !!sym(y_var))) +
      geom_point() +
      labs(x = x_var, y = y_var) +
      theme_minimal()
  })
  
  # Classification with Decision Tree or Logistic Regression
  accuracy <- reactiveValues(dt = NULL, lr = NULL)
  
  observeEvent(input$classify_button, {
    req(input$file)
    req(input$x_var)
    req(input$y_var)
    req(input$classification_algorithm)
    
    if (input$classification_algorithm == "Decision Tree") {
      data_processed <- data() %>%
        mutate(y_category = cut(.data[[input$y_var]], breaks = 3, labels = c("Low", "Medium", "High")))
      dt_model <- rpart(y_category ~ ., data = data_processed)
      prediction <- predict(dt_model, newdata = data_processed, type = "class")
      accuracy$dt <- sum(prediction == data_processed$y_category) / nrow(data_processed)
      output$classification_result <- renderPrint({
        cat("Decision Tree Model Summary:\n")
        print(summary(dt_model))
        cat("\nAccuracy:", accuracy$dt, "\n")
      })
    } else if (input$classification_algorithm == "Logistic Regression") {
      threshold <- 0.5
      df <- data() %>%
        mutate(y_category = cut(.data[[input$y_var]], breaks = 3, labels = c("Low", "Medium", "High")))
      lr_model <- glm(formula = as.formula(paste("y_category ~ .")), data = df, family = binomial, maxit = 1000)
      prediction <- predict(lr_model, newdata = df, type = "response")
      prediction <- ifelse(prediction > threshold, "High", ifelse(prediction < threshold, "Low", "Medium"))
      accuracy$lr <- sum(prediction == df$y_category) / nrow(df)
      output$classification_result <- renderPrint({
        cat("Logistic Regression Model Summary:\n")
        print(summary(lr_model))
        cat("\nAccuracy:", accuracy$lr, "\n")
      })
    }
  })
  
  # Comparison of algorithm accuracies
  output$accuracy_comparison <- renderPrint({
    req(input$classify_button)
    
    if (!is.null(accuracy$dt) && !is.null(accuracy$lr)) {
      if (accuracy$dt > accuracy$lr) {
        cat("Decision Tree έχει υψηλότερη ακρίβεια.\n")
      } else if (accuracy$dt < accuracy$lr) {
        cat("Logistic Regression έχει υψηλότερη ακρίβεια.\n")
      } else {
        cat("Και οι δύο αλγόριθμοι έχουν την ίδια ακρίβεια.\n")
      }
    }
  })
  
  # Clustering with K-Means or Hierarchical Clustering
  silhouette_scores <- reactiveValues(kmeans = NULL, hclust = NULL)
  
  observeEvent(input$run_clustering, {
    req(input$file)
    req(input$x_var)
    req(input$y_var)
    req(input$clustering_algorithm)
    
    if (input$clustering_algorithm == "K-Means") {
      kmeans_result <- kmeans(data()[, c(input$x_var, input$y_var)], centers = 3)
      cluster_labels <- kmeans_result$cluster
      silhouette_score <- silhouette(cluster_labels, dist(data()[, c(input$x_var, input$y_var)]))
      silhouette_scores$kmeans <- mean(silhouette_score[, "sil_width"])
      output$clustering_result <- renderPrint({
        cat("K-Means Clustering Summary:\n")
        print(kmeans_result)
        cat("\nSilhouette Score:", silhouette_scores$kmeans, "\n")
      })
    } else if (input$clustering_algorithm == "Hierarchical Clustering") {
      hclust_result <- hclust(dist(data()[, c(input$x_var, input$y_var)]))
      cluster_labels <- cutree(hclust_result, k = 3)
      silhouette_score <- silhouette(cluster_labels, dist(data()[, c(input$x_var, input$y_var)]))
      silhouette_scores$hclust <- mean(silhouette_score[, "sil_width"])
      output$clustering_result <- renderPrint({
        cat("Hierarchical Clustering Summary:\n")
        print(hclust_result)
        cat("\nSilhouette Score:", silhouette_scores$hclust, "\n")
      })
    }
    
    # Comparison of algorithm silhouette scores
    if (!is.null(silhouette_scores$kmeans) && !is.null(silhouette_scores$hclust)) {
      output$silhouette_comparison <- renderPrint({
        if (silhouette_scores$kmeans > silhouette_scores$hclust) {
          cat("K-Means έχει υψηλότερο Silhouette Score.\n")
        } else if (silhouette_scores$kmeans < silhouette_scores$hclust) {
          cat("Hierarchical Clustering έχει υψηλότερο Silhouette Score.\n")
        } else {
          cat("Και οι δύο αλγόριθμοι έχουν το ίδιο Silhouette Score.\n")
        }
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
