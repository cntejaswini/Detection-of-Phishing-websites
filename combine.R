# Install necessary packages if not already installed
required_packages <- c("data.table", "dplyr", "caret", "Metrics", "e1071", "rpart", "randomForest", "class", "ggplot2", "reshape2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load necessary libraries
library(data.table)
library(dplyr)
library(caret)
library(Metrics)
library(e1071)
library(rpart)
library(randomForest)
library(class)
library(ggplot2)
library(reshape2)

process <- function(path) {
  colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#8c564b")
  
  # Load the dataset
  df <- fread(path)
  setnames(df, old = names(df), new = c('having_IP_Address', 'URL_Length', 'Shortining_Service', 
                                        'having_At_Symbol', 'double_slash_redirecting', 'Prefix_Suffix', 
                                        'having_Sub_Domain', 'SSLfinal_State', 'Domain_registeration_length', 
                                        'Favicon', 'port', 'HTTPS_token', 'Request_URL', 'URL_of_Anchor', 
                                        'Links_in_tags', 'SFH', 'Submitting_to_email', 'Abnormal_URL', 
                                        'Redirect', 'on_mouseover', 'RightClick', 'popUpWidnow', 'Iframe', 
                                        'age_of_domain', 'DNSRecord', 'web_traffic', 'Page_Rank', 
                                        'Google_Index', 'Links_pointing_to_page', 'Statistical_report', 'Result'))
  
  X <- df[, !c("Result"), with = FALSE]
  y <- df$Result
  
  # Decision Tree classifier
  dtree <- rpart(as.factor(y) ~ ., data = df, method = "class")
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
  rfe_results <- rfe(X, y, sizes = c(3), rfeControl = control)
  
  # Summarize the selection of the attributes
  print(rfe_results)
  print(rfe_results$optVariables)
  
  # Features sorted by their rank
  sorted_rfe <- rfe_results$optVariables
  cols <- sorted_rfe[1:3]
  
  X_new <- X[, ..cols]
  set.seed(123)
  trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
  X_train <- X_new[trainIndex, ]
  X_test <- X_new[-trainIndex, ]
  y_train <- y[trainIndex]
  y_test <- y[-trainIndex]
  
  # Fit and evaluate models
  dtree_model <- rpart(as.factor(y_train) ~ ., data = as.data.frame(X_train), method = "class")
  dtree_pred <- predict(dtree_model, newdata = as.data.frame(X_test), type = "class")
  DTac <- mean(dtree_pred == y_test)
  
  svc_model <- svm(as.factor(y_train) ~ ., data = as.data.frame(X_train), kernel = "linear", cost = 1)
  svc_pred <- predict(svc_model, newdata = as.data.frame(X_test))
  SVMac <- mean(svc_pred == y_test)
  
  rf_model <- randomForest(as.factor(y_train) ~ ., data = as.data.frame(X_train))
  rf_pred <- predict(rf_model, newdata = as.data.frame(X_test))
  RFac <- mean(rf_pred == y_test)
  
  lr_model <- glm(as.factor(y_train) ~ ., data = as.data.frame(X_train), family = binomial)
  lr_pred <- predict(lr_model, newdata = as.data.frame(X_test), type = "response")
  lr_pred <- ifelse(lr_pred > 0.5, 1, 0)
  LRac <- mean(lr_pred == y_test)
  
  knn_pred <- knn(train = as.data.frame(X_train), test = as.data.frame(X_test), cl = y_train, k = 4)
  KNNac <- mean(knn_pred == y_test)
  
  # Write results to CSV
  result2 <- data.frame(Parameter = c("DT", "SVM", "RF", "LR", "KNN"), Value = c(DTac, SVMac, RFac, LRac, KNNac))
  fwrite(result2, "results/CombineAccuracy.csv", row.names = FALSE)
  
  # Plot accuracy values
  ggplot(result2, aes(x = Parameter, y = Value, fill = Parameter)) + 
    geom_bar(stat = "identity") +
    xlab('Parameter') +
    ylab('Value') +
    ggtitle('Accuracy Value') +
    ggsave('results/AccuracyValue.png')
}

# Example usage
dir.create("results", showWarnings = FALSE)
process("C://Users//cntej//Downloads//phising//phising//data.csv")

