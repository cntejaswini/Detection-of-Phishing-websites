# Load necessary libraries
required_packages <- c("data.table", "dplyr", "randomForest", "caret", "ggplot2", "e1071")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

process <- function(path, inputurl) {
  # Read the CSV file
  df <- fread(path)
  
  # Splitting the dataset into training set and test set
  set.seed(123) # For reproducibility
  trainIndex <- createDataPartition(df$Result, p = 0.75, list = FALSE)
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  
  x_train <- trainData[, -ncol(trainData)]
  y_train <- trainData$Result
  x_test <- testData[, -ncol(testData)]
  y_test <- testData$Result
  
  # Applying grid search to find best performing parameters
  control <- trainControl(method = "cv", number = 5)
  tunegrid <- expand.grid(.mtry = c(2, 3, 4))
  
  rf_gridsearch <- train(as.factor(y_train) ~ ., data = trainData, method = "rf", 
                         tuneGrid = tunegrid, trControl = control)
  
  print(paste("Best Accuracy =", max(rf_gridsearch$results$Accuracy)))
  print(paste("Best Parameters =", paste(rf_gridsearch$bestTune, collapse = " ")))
  
  # Fitting Random Forest with best parameters
  best_mtry <- rf_gridsearch$bestTune$.mtry
  classifier <- randomForest(as.factor(y_train) ~ ., data = trainData, mtry = best_mtry, 
                             ntree = 100, importance = TRUE)
  
  # Predicting the test set result
  y_pred <- predict(classifier, newdata = x_test)
  
  # Confusion matrix
  cm <- table(y_test, y_pred)
  print(cm)
  
  # Save the model
  saveRDS(classifier, "results/rf_final.rds")
  
  # Features Importance
  importance <- importance(classifier)
  var_imp <- data.frame(Importance = importance[order(importance, decreasing = TRUE), ])
  var_imp$Features <- rownames(var_imp)
  
  # Plotting variable importance
  ggplot(var_imp, aes(x = reorder(Features, Importance), y = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Variable Importances", x = "Features", y = "Importance") +
    theme_minimal() +
    ggsave("results/RFRelativeImportance.png")
  
  # Save predictions to CSV
  pred_df <- data.frame(ID = 1:length(y_pred), Predicted_Value = y_pred)
  fwrite(pred_df, "results/resultRF.csv")
  
  # Calculate metrics
  mse <- mean((y_test - as.numeric(y_pred))^2)
  mae <- mean(abs(y_test - as.numeric(y_pred)))
  r2 <- cor(y_test, as.numeric(y_pred))^2
  rmse <- sqrt(mse)
  accuracy <- mean(y_test == y_pred)
  
  print(paste("MSE:", mse))
  print(paste("MAE:", mae))
  print(paste("R-squared:", r2))
  print(paste("RMSE:", rmse))
  print(paste("Accuracy:", accuracy))
  
  # Save metrics to CSV
  metrics_df <- data.frame(Parameter = c("MSE", "MAE", "R-SQUARED", "RMSE", "ACCURACY"),
                           Value = c(mse, mae, r2, rmse, accuracy))
  fwrite(metrics_df, "results/RFMetrics.csv")
  
  # Plot metrics
  ggplot(metrics_df, aes(x = Parameter, y = Value, fill = Parameter)) +
    geom_bar(stat = "identity") +
    labs(title = "Random Forest Metrics Value") +
    theme_minimal() +
    ggsave("results/RFMetricsValue.png")
  
  # Load the model
  classifier <- readRDS("results/rf_final.rds")
  
  # Check and predict
  checkprediction <- inputScript_process(inputurl)  # Assuming inputScript_process is defined in R
  prediction <- predict(classifier, newdata = checkprediction)
  print(prediction)
  return(prediction)
}

# Example usage
process("C:\\Users\\cntej\\Downloads\\phising\\phising\\data.csv")