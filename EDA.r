# Load necessary libraries
library(tidyverse)

# Read the Titanic dataset
titanic <- read.csv("../input/titanic/train.csv", header = TRUE, sep = ",")

# Display the last 10 rows of the dataset
tail(titanic, 10)

# Display the first 10 rows of the dataset
head(titanic, 10)

# Get column names
names(titanic)

# Check the structure of the dataset
str(titanic)

# Summary statistics for numerical columns
summary(titanic)

# Five-number summary for Fare and Age
fivenum(titanic$Fare)
fivenum(titanic$Age)

# Mean and median for Fare and Age
mean(titanic$Fare)
mean(titanic$Age, na.rm = TRUE)
median(titanic$Fare)
median(titanic$Age, na.rm = TRUE)

# Quantiles for Age
quantile(titanic$Age, na.rm = TRUE)

# Minimum and maximum Age values
min(titanic$Age, na.rm = TRUE)
max(titanic$Age, na.rm = TRUE)

# Histogram of Age
hist(titanic$Age, main = "Age Distribution", xlab = "Age")

# Unique values in the Age column
unique(titanic$Age)

# Sort Age values in ascending order
sort(titanic$Age, decreasing = FALSE)

# Frequency table for the Survived column
table(titanic$Survived)

# Bar plot for the Survived column
barplot(table(titanic$Survived), main = "Survival Count")

# Cross-tabulation of Pclass and Survived
table(titanic$Pclass, titanic$Survived)

# Box plot of Age by Pclass
boxplot(Age ~ Pclass, data = titanic, main = "Age Distribution by Pclass")

# Scatter plot of Age vs. Fare
plot(titanic$Age, titanic$Fare, main = "Age vs. Fare", xlab = "Age", ylab = "Fare")

# Correlation matrix
cor(titanic$Age, titanic$Fare)
