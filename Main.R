# Install necessary packages if not already installed
required_packages <- c("shiny", "shinyWidgets", "caret", "data.table", "randomForest", "e1071", "rpart", "class", "ggplot2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

library(shiny)
library(shinyWidgets)
library(caret)
library(data.table)
library(randomForest)
library(e1071)
library(rpart)
library(class)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Detecting Phishing Website"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = c(".csv")),
      textInput("url", "Enter URL"),
      actionButton("clear", "Clear"),
      br(),
      br(),
      actionButton("rf", "Run Random Forest"),
      actionButton("lr", "Run Logistic Regression"),
      actionButton("svm", "Run SVM"),
      actionButton("knn", "Run KNN"),
      actionButton("dt", "Run Decision Tree"),
      actionButton("predict", "Predict"),
      actionButton("compare", "Comparison")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results", tableOutput("results")),
        tabPanel("Metrics", tableOutput("metrics")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  metrics <- reactiveVal(NULL)
  plotData <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    dataset(fread(input$file1$datapath))
  })
  
  observeEvent(input$clear, {
    dataset(NULL)
    results(NULL)
    metrics(NULL)
    plotData(NULL)
    updateTextInput(session, "url", value = "")
  })
  
  runModel <- function(modelFunc) {
    if (is.null(dataset())) {
      showModal(modalDialog(
        title = "Error",
        "Please upload a dataset first.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    data <- dataset()
    setnames(data, old = names(data), new = c('having_IP_Address', 'URL_Length', 'Shortining_Service', 
                                              'having_At_Symbol', 'double_slash_redirecting', 'Prefix_Suffix', 
                                              'having_Sub_Domain', 'SSLfinal_State', 'Domain_registeration_length', 
                                              'Favicon', 'port', 'HTTPS_token', 'Request_URL', 'URL_of_Anchor', 
                                              'Links_in_tags', 'SFH', 'Submitting_to_email', 'Abnormal_URL', 
                                              'Redirect', 'on_mouseover', 'RightClick', 'popUpWidnow', 'Iframe', 
                                              'age_of_domain', 'DNSRecord', 'web_traffic', 'Page_Rank', 
                                              'Google_Index', 'Links_pointing_to_page', 'Statistical_report', 'Result'))
    
    X <- data[, !c("Result"), with = FALSE]
    y <- data$Result
    
    set.seed(123)
    trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
    X_train <- X[trainIndex, ]
    X_test <- X[-trainIndex, ]
    y_train <- y[trainIndex]
    y_test <- y[-trainIndex]
    
    model <- modelFunc(X_train, y_train)
    y_pred <- predict(model, newdata = X_test)
    
    mse <- mean((y_test - y_pred)^2)
    mae <- mean(abs(y_test - y_pred))
    r2 <- R2(y_test, y_pred)
    accuracy <- mean(y_test == y_pred)
    
    metrics(data.frame(
      Parameter = c("MSE", "MAE", "R-SQUARED", "Accuracy"),
      Value = c(mse, mae, r2, accuracy)
    ))
    
    results(data.frame(ID = 1:length(y_pred), Predicted_Value = y_pred))
  }
  
  observeEvent(input$rf, {
    runModel(function(X, y) randomForest(X, y))
  })
  
  observeEvent(input$lr, {
    runModel(function(X, y) glm(as.factor(y) ~ ., data = X, family = binomial))
  })
  
  observeEvent(input$svm, {
    runModel(function(X, y) svm(X, y, kernel = "linear"))
  })
  
  observeEvent(input$knn, {
    runModel(function(X, y) knn(train = X, test = X, cl = y, k = 4))
  })
  
  observeEvent(input$dt, {
    runModel(function(X, y) rpart(as.factor(y) ~ ., data = X, method = "class"))
  })
  
  observeEvent(input$predict, {
    showModal(modalDialog(
      title = "Prediction",
      "Prediction functionality is not yet implemented.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$compare, {
    showModal(modalDialog(
      title = "Comparison",
      "Comparison functionality is not yet implemented.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$results <- renderTable({
    results()
  })
  
  output$metrics <- renderTable({
    metrics()
  })
  
  output$plot <- renderPlot({
    plotData <- metrics()
    if (!is.null(plotData)) {
      ggplot(plotData, aes(x = Parameter, y = Value, fill = Parameter)) +
        geom_bar(stat = "identity") +
        xlab('Parameter') +
        ylab('Value') +
        ggtitle('Metrics Value')
    }
  })
}

shinyApp(ui = ui, server = server)
