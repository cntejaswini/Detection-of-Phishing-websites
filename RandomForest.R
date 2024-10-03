# Install necessary packages if not already installed
required_packages <- c("data.table", "dplyr", "caret", "Metrics", "randomForest", "RFE", "ggplot2", "seaborn")

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
library(randomForest)
library(ggplot2)
library(reshape2)

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
  
  rf <- randomForest(X, y)
  rfe <- rfe(X, y, sizes = c(1:3), rfeControl = rfeControl(functions = rfFuncs, method = "cv", number = 10))
  
  # Summarize the selection of the attributes
  print(rfe)
  print(rfe$optVariables)
  
  # Plotting variable importance
  importance <- varImp(rf)
  ggplot(importance, aes(x = reorder(Variables, Overall), y = Overall)) + 
    geom_bar(stat = 'identity', fill = colors[1]) +
    coord_flip() +
    xlab('Feature Selection') +
    ylab('RANK') +
    ggtitle('Feature Selection BY RandomForest') +
    ggsave('results/RFFeatureSelection.png')
  
  cols <- rfe$optVariables
  
  # Pairplot
  df_pairs <- df[, ..cols]
  pairs(df_pairs)
  ggsave('results/RFPairplot.png')
  
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
    ggsave('results/RFHeatmap.png')
  
  # Fit model with selected features
  X_new <- X[, ..cols]
  set.seed(123)
  trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
  X_train <- X_new[trainIndex, ]
  X_test <- X_new[-trainIndex, ]
  y_train <- y[trainIndex]
  y_test <- y[-trainIndex]
  
  rf.fit <- randomForest(X_train, y_train)
  y_pred <- predict(rf.fit, X_test)
  
  mse <- mse(y_test, y_pred)
  mae <- mae(y_test, y_pred)
  r2 <- R2(y_test, y_pred)
  rms <- sqrt(mse)
  ac <- accuracy(y_test, y_pred)
  
  print("------------------------------------------------------------------")	
  print(paste("MSE VALUE FOR Random Forest IS", mse))
  print(paste("MAE VALUE FOR Random Forest IS", mae))
  print(paste("R-SQUARED VALUE FOR Random Forest IS", r2))
  print(paste("RMSE VALUE FOR Random Forest IS", rms))
  print(paste("ACCURACY VALUE Random Forest IS", ac))
  print("------------------------------------------------------------------")	
  
  result2 <- data.frame(ID = 1:length(y_pred), Predicted_Value = y_pred)
  write.csv(result2, "results/resultRandomForest.csv", row.names = FALSE)
  
  metrics_df <- data.frame(Parameter = c("MSE", "MAE", "R-SQUARED", "RMSE", "ACCURACY"),
                           Value = c(mse, mae, r2, rms, ac))
  write.csv(metrics_df, "results/RFMetrics.csv", row.names = FALSE)
  
  ggplot(metrics_df, aes(x = Parameter, y = Value, fill = Parameter)) + 
    geom_bar(stat = 'identity') +
    xlab('Parameter') +
    ylab('Value') +
    ggtitle('Random Forest Metrics Value') +
    ggsave('results/RFMetricsValue.png')
}

# Example usage
dir.create("results", showWarnings = FALSE)
process(""C:\\Users\\cntej\\Downloads\\phising\\phising\\data.csv"")