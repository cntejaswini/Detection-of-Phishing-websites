# Install necessary packages if not already installed
required_packages <- c("data.table", "dplyr", "caret", "Metrics", "class", "randomForest", "ggplot2", "mlxtend")

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
library(class)
library(ggplot2)
library(mlxtend)

process <- function(path) {
  colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#8c564b")
  explode <- c(0.1, 0, 0, 0, 0)  
  
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
  
  # KNN classifier
  knn <- train(X, y, method = "knn", tuneGrid = expand.grid(k = 4), trControl = trainControl(method = "none"))
  
  # Sequential feature selection
  sfs_control <- rfeControl(functions = caretFuncs, method = "cv", number = 10)
  sfs_results <- rfe(X, y, sizes = c(1:3), rfeControl = sfs_control, method = "knn")
  
  # Summarize the selection of the attributes
  print(sfs_results)
  print(sfs_results$optVariables)
  
  # Features sorted by their rank
  sorted_sfs <- sfs_results$optVariables
  cols <- sorted_sfs[1:3]
  
  # Barplot for the dependent variable
  feature_ranks <- data.frame(Feature = cols, Rank = 1:length(cols))
  ggplot(feature_ranks, aes(x = reorder(Feature, Rank), y = Rank, fill = Feature)) + 
    geom_bar(stat = "identity", alpha = 0.5) +
    xlab('Feature Selection') +
    ylab('RANK') +
    ggtitle("Feature Selection BY KNN") +
    ggsave('results/KNNFeatureSelection.png')
  
  # Fit model with selected features
  X_new <- X[, ..cols]
  set.seed(123)
  trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
  X_train <- X_new[trainIndex, ]
  X_test <- X_new[-trainIndex, ]
  y_train <- y[trainIndex]
  y_test <- y[-trainIndex]
  
  knn_model <- knn(train = X_train, test = X_test, cl = y_train, k = 4)
  y_pred <- as.numeric(as.character(knn_model))
  
  mse <- mean_squared_error(y_test, y_pred)
  mae <- mean_absolute_error(y_test, y_pred)
  r2 <- R2(y_test, y_pred)
  rms <- sqrt(mse)
  ac <- accuracy_score(y_test, y_pred)
  
  print("------------------------------------------------------------------")	
  print(paste("MSE VALUE FOR KNN IS", mse))
  print(paste("MAE VALUE FOR KNN IS", mae))
  print(paste("R-SQUARED VALUE FOR KNN IS", r2))
  print(paste("RMSE VALUE FOR KNN IS", rms))
  print(paste("ACCURACY VALUE KNN IS", ac))
  print("------------------------------------------------------------------")	
  
  result2 <- data.frame(ID = 1:length(y_pred), Predicted_Value = y_pred)
  write.csv(result2, "results/resultKNN.csv", row.names = FALSE)
  
  metrics_df <- data.frame(Parameter = c("MSE", "MAE", "R-SQUARED", "RMSE", "ACCURACY"),
                           Value = c(mse, mae, r2, rms, ac))
  write.csv(metrics_df, "results/KNNMetrics.csv", row.names = FALSE)
  
  ggplot(metrics_df, aes(x = Parameter, y = Value, fill = Parameter)) + 
    geom_bar(stat = 'identity') +
    xlab('Parameter') +
    ylab('Value') +
    ggtitle('KNN Metrics Value') +
    ggsave('results/KNNMetricsValue.png')
}


dir.create("results", showWarnings = FALSE)
process(""C:\\Users\\cntej\\Downloads\\phising\\phising\\data.csv"")

