# Install necessary packages if not already installed
required_packages <- c("data.table", "dplyr", "caret", "Metrics")

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

process <- function(path) {
  # Read the CSV file
  df <- fread(path)
  setnames(df, old = names(df), new = c('having_IP_Address', 'URL_Length', 'Shortining_Service', 
                                        'having_At_Symbol', 'double_slash_redirecting', 'Prefix_Suffix', 
                                        'having_Sub_Domain', 'SSLfinal_State', 'Domain_registeration_length', 
                                        'Favicon', 'port', 'HTTPS_token', 'Request_URL', 'URL_of_Anchor', 
                                        'Links_in_tags', 'SFH', 'Submitting_to_email', 'Abnormal_URL', 
                                        'Redirect', 'on_mouseover', 'RightClick', 'popUpWidnow', 'Iframe', 
                                        'age_of_domain', 'DNSRecord', 'web_traffic ', 'Page_Rank', 
                                        'Google_Index', 'Links_pointing_to_page', 'Statistical_report', 'Result'))
  
  X <- df[, !'Result', with = FALSE]
  y <- df$Result
  
  # Split data into training and testing sets
  set.seed(123) # For reproducibility
  trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
  X_train <- X[trainIndex, ]
  X_test <- X[-trainIndex, ]
  y_train <- y[trainIndex]
  y_test <- y[-trainIndex]
  
  # Fit logistic regression model
  lr <- train(as.factor(y_train) ~ ., data = X_train, method = "glm", family = "binomial")
  y_pred <- predict(lr, newdata = X_test)
  
  # Calculate metrics
  mse <- mse(y_test, as.numeric(y_pred))
  mae <- mae(y_test, as.numeric(y_pred))
  r2 <- R2(y_test, as.numeric(y_pred))
  accuracy <- accuracy(y_test, as.numeric(y_pred))
  
  print(paste("MSE:", mse))
  print(paste("MAE:", mae))
  print(paste("R-squared:", r2))
  print(paste("Accuracy:", accuracy))
  
  # Write predictions to CSV file
  pred_df <- data.frame(ID = 1:length(y_pred), Predicted_Value = as.numeric(y_pred))
  write.csv(pred_df, "results/resultLogisticRegression.csv", row.names = FALSE)
  
  # Write metrics to CSV file
  metrics_df <- data.frame(Parameter = c("MSE", "MAE", "R-SQUARED", "ACCURACY"),
                           Value = c(mse, mae, r2, accuracy))
  write.csv(metrics_df, "results/LRMetrics.csv", row.names = FALSE)
}

# Example usage
# process(""C:\\Users\\cntej\\Downloads\\phising\\phising\\data.csv"")