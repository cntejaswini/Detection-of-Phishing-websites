# Install necessary packages if not already installed
required_packages <- c("data.table", "dplyr", "caret", "Metrics", "e1071", "randomForest", "ggplot2", "reshape2")

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
library(ggplot2)
library(reshape2)
library(gridExtra)

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
  
  # Create the RFE object and rank each pixel
  svc <- svm(X, y, kernel = "linear", cost = 1)
  rfe_control <- rfeControl(functions = caretFuncs, method = "cv", number = 10)
  rfe_results <- rfe(X, y, sizes = c(1:3), rfeControl = rfe_control)
  
  # Summarize the selection of the attributes
  print(rfe_results)
  print(rfe_results$optVariables)
  
  # Features sorted by their rank
  sorted_rfe <- rfe_results$optVariables
  cols <- sorted_rfe[1:3]
  
  # Barplot for the dependent variable
  feature_ranks <- data.frame(Feature = cols, Rank = 1:length(cols))
  ggplot(feature_ranks, aes(x = reorder(Feature, Rank), y = Rank, fill = Feature)) + 
    geom_bar(stat = "identity", alpha = 0.5) +
    xlab('Feature Selection') +
    ylab('RANK') +
    ggtitle("Feature Selection BY SVM") +
    ggsave('results/SVMFeatureSelection.png')
  
  # Pairplot
  df_pairs <- df[, ..cols]
  pairs(df_pairs)
  ggsave('results/SVMPairplot.png')
  
  # Heatmap
  cor_mat <- cor(df_pairs)
  melt_cor <- melt(cor_mat)
  ggplot(data = melt_cor, aes(Var1, Var2, fill = value)) + 
    geom_tile() +
    geom_text(aes(label = round(value, 2)), size = 4) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) +
    coord_fixed() +
    ggtitle('Correlation Matrix') +
    ggsave('results/SVMHeatmap.png')
  
  # Fit model with selected features
  X_new <- X[, ..cols]
  set.seed(123)
  trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
  X_train <- X_new[trainIndex, ]
  X_test <- X_new[-trainIndex, ]
  y_train <- y[trainIndex]
  y_test <- y[-trainIndex]
  
  svc <- svm(X_train, y_train, kernel = "linear", cost = 1)
  y_pred <- predict(svc, X_test)
  
  mse <- mean_squared_error(y_test, y_pred)
  mae <- mean_absolute_error(y_test, y_pred)
  r2 <- R2(y_test, y_pred)
  rms <- sqrt(mse)
  ac <- accuracy_score(y_test, y_pred)
  
  print("------------------------------------------------------------------")	
  print(paste("MSE VALUE FOR SVM IS", mse))
  print(paste("MAE VALUE FOR SVM IS", mae))
  print(paste("R-SQUARED VALUE FOR SVM IS", r2))
  print(paste("RMSE VALUE FOR SVM IS", rms))
  print(paste("ACCURACY VALUE SVM IS", ac))
  print("------------------------------------------------------------------")	
  
  result2 <- data.frame(ID = 1:length(y_pred), Predicted_Value = y_pred)
  write.csv(result2, "results/resultSVM.csv", row.names = FALSE)
  
  metrics_df <- data.frame(Parameter = c("MSE", "MAE", "R-SQUARED", "RMSE", "ACCURACY"),
                           Value = c(mse, mae, r2, rms, ac))
  write.csv(metrics_df, "results/SVMMetrics.csv", row.names = FALSE)
  
  ggplot(metrics_df, aes(x = Parameter, y = Value, fill = Parameter)) + 
    geom_bar(stat = 'identity') +
    xlab('Parameter') +
    ylab('Value') +
    ggtitle('SVM Metrics Value') +
    ggsave('results/SVMMetricsValue.png')
}

# Example usage
dir.create("results", showWarnings = FALSE)
process(""C:\\Users\\cntej\\Downloads\\phising\\phising\\data.csv"")