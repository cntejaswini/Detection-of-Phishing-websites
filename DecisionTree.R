# Install necessary packages if not already installed
required_packages <- c("data.table", "dplyr", "caret", "Metrics", "ggplot2", "e1071", "rpart", "reshape2", "randomForest")

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
library(ggplot2)
library(reshape2)
library(rpart)
library(e1071)
library(randomForest)

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
  
  # Barplot for the dependent variable
  feature_ranks <- data.frame(Feature = cols, Rank = 1:length(cols))
  ggplot(feature_ranks, aes(x = reorder(Feature, Rank), y = Rank, fill = Feature)) + 
    geom_bar(stat = "identity", alpha = 0.5) +
    xlab('Feature Selection') +
    ylab('RANK') +
    ggtitle("Feature Selection BY DecisionTree") +
    ggsave('results/DTFeatureSelection.png')
  
  # Pairplot
  df_pairs <- df[, ..cols]
  pairs(df_pairs)
  ggsave('results/DTPairplot.png')
  
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
    ggsave('results/DTHeatmap.png')
  
  # Fit model with selected features
  X_new <- X[, ..cols]
  set.seed(123)
  trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
  X_train <- X_new[trainIndex, ]
  X_test <- X_new[-trainIndex, ]
  y_train <- y[trainIndex]
  y_test <- y[-trainIndex]
  
  dtree_model <- rpart(as.factor(y_train) ~ ., data = as.data.frame(X_train), method = "class")
  y_pred <- predict(dtree_model, newdata = as.data.frame(X_test), type = "class")
  
  mse <- mean_squared_error(y_test, as.numeric(y_pred))
  mae <- mean_absolute_error(y_test, as.numeric(y_pred))
  r2 <- R2(y_test, as.numeric(y_pred))
  rms <- sqrt(mse)
  ac <- mean(y_pred == y_test)
  
  print(paste("MSE VALUE FOR DecisionTree IS", mse))
  print(paste("MAE VALUE FOR DecisionTree IS", mae))
  print(paste("R-SQUARED VALUE FOR DecisionTree IS", r2))
  print(paste("RMSE VALUE FOR DecisionTree IS", rms))
  print(paste("ACCURACY VALUE FOR DecisionTree IS", ac))
  print("------------------------------------------------------------------")	
  
  # Save predictions to CSV
  pred_df <- data.frame(ID = 1:length(y_pred), Predicted_Value = y_pred)
  fwrite(pred_df, "results/resultDecisionTree.csv")
  
  # Save metrics to CSV
  metrics_df <- data.frame(Parameter = c("MSE", "MAE", "R-SQUARED", "RMSE", "ACCURACY"),
                           Value = c(mse, mae, r2, rms, ac))
  fwrite(metrics_df, "results/DTMetrics.csv")
  
  # Plot metrics
  ggplot(metrics_df, aes(x = Parameter, y = Value, fill = Parameter)) + 
    geom_bar(stat = "identity") +
    xlab('Parameter') +
    ylab('Value') +
    ggtitle('Decision Tree Metrics Value') +
    ggsave('results/DTMetricsValue.png')
}

# Example usage
# Make sure to replace the file paths with the appropriate paths to your data files
 process("C://Users//cntej//Downloads//phising//phising//data.csv")

 