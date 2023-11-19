# Install and load necessary packages if not already installed
if (!requireNamespace("class", quietly = TRUE)) {
  install.packages("class")
}
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}
if (!requireNamespace("rpart.plot", quietly = TRUE)) {
  install.packages("rpart.plot")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}

# Load required libraries
library(class)
library(rpart)
library(rpart.plot)
library(caret)

# Read the data
wine_data <- read.csv("/kaggle/input/red-wine-quality-cortez-et-al-2009/winequality-red.csv", sep = ",", header = TRUE)

# Create columns for "good," "poor," and "okay"
wine_data$poor <- wine_data$quality <= 4
wine_data$okay <- wine_data$quality == 5 | wine_data$quality == 6
wine_data$good <- wine_data$quality >= 7

# Display the structure of the data
str(wine_data)

# Display summary statistics
summary(wine_data)

# Set seed for reproducibility
set.seed(123)

# KNN with k = 10
class_knn10 = knn(train = wine_data[,1:11], test = wine_data[,1:11], cl = wine_data$good, k = 10)

# KNN with k = 20
class_knn20 = knn(train = wine_data[,1:11], test = wine_data[,1:11], cl = wine_data$good, k = 20)

# Create confusion matrix for KNN with k = 10
conf_matrix_knn10 <- table(wine_data$good, class_knn10)
print(conf_matrix_knn10)

# Create confusion matrix for KNN with k = 20
conf_matrix_knn20 <- table(wine_data$good, class_knn20)
print(conf_matrix_knn20)

# Calculate accuracy for KNN with k = 10
accuracy_knn10 <- sum(diag(conf_matrix_knn10)) / sum(conf_matrix_knn10)
cat("Accuracy for KNN with k = 10:", accuracy_knn10, "\n")

# Calculate accuracy for KNN with k = 20
accuracy_knn20 <- sum(diag(conf_matrix_knn20)) / sum(conf_matrix_knn20)
cat("Accuracy for KNN with k = 20:", accuracy_knn20, "\n")
