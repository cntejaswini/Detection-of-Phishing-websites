# Machine Learning Models for Phishing Website Detection

## Project Overview

This project focuses on detecting phishing websites using multiple machine learning models. Various algorithms, including Logistic Regression, K-Nearest Neighbors (KNN), Decision Trees, Random Forest, Support Vector Machines (SVM), and more, are used to classify websites as phishing or legitimate based on several features.

## Dataset

The project uses a dataset (`data.csv`) that includes various attributes related to website characteristics, which are used to determine whether a site is phishing or legitimate. Each row corresponds to a website, and the features include several technical aspects that differentiate phishing sites from legitimate ones.

## Project Structure

- **InputScript.R**: Script for initial data processing and loading the dataset.
- **LogisticRegression.R**: Script that implements the logistic regression model.
- **KNN.R**: Script that implements the K-Nearest Neighbors algorithm for classification.
- **DecisionTree.R**: Script that implements the decision tree algorithm.
- **RandomForest.R**: Script that implements the random forest algorithm for classification.
- **SVM.R**: Script that implements the Support Vector Machines model.
- **combine.R**: Script to combine the predictions from multiple models and evaluate overall performance.
- **Main.R**: Main script to run all the models and compare performance.
- **data.csv**: Dataset file that contains the features and labels used for training the machine learning models.

## Key Steps in the Project

1. **Data Preprocessing**:
   - Loaded and cleaned the dataset.
   - Handled missing values, if any.
   - Performed feature scaling and encoding where necessary.

2. **Model Training**:
   - Built multiple machine learning models including Logistic Regression, KNN, Decision Trees, Random Forest, and SVM.
   - Used cross-validation to evaluate model performance.

3. **Model Evaluation**:
   - Evaluated models using accuracy, precision, recall, and F1-score.
   - Identified the best-performing model based on overall metrics.

4. **Ensemble Learning**:
   - Combined results from different models to improve prediction performance.

## How to Run

1. **Clone the repository**:
   ```bash
   git clone https://github.com/cntejaswini/Detection-of-Phishing-websites.git
2. Set up the environment: Make sure you have R and all necessary libraries installed.

3. Run the scripts:

    Start with InputScript.R to preprocess the data.
    Run individual scripts for each machine learning model (e.g., LogisticRegression.R, KNN.R, etc.).
    Use Main.R to execute all models and evaluate the overall performance.
## Libraries and Dependencies
Make sure to install the following R libraries before running the project:
install.packages("caret")
install.packages("e1071")
install.packages("randomForest")
install.packages("nnet")

## Results and Conclusion
After training and evaluating all models, the Random Forest algorithm (or whichever is the best) provided the highest accuracy in detecting phishing websites. Combining multiple models further improved the prediction results.
