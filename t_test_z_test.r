# Load the necessary libraries
library(tidyverse) # metapackage of all tidyverse packages
install.packages("BSDA") # Install the library for statistical analysis

# Load the heart disease dataset
heart_data <- read.csv("/kaggle/input/heart-disease-cleveland-uci/heart_cleveland_upload.csv")

# Display the first few rows of the dataset
head(heart_data)

# Separate the data into two groups: patients with heart disease and patients without heart disease
heart_disease <- subset(heart_data, condition == 1)
no_heart_disease <- subset(heart_data, condition == 0)

# Perform a two-sample t-test to compare the mean ages of the two groups
t_test_result <- t.test(heart_disease$age, no_heart_disease$age)

# Display the t-test result
t_test_result

# Install the BSDA library for additional statistical analysis
install.packages("BSDA")

# Conduct a z-test for the difference between means of cholesterol levels in the two groups
library("BSDA")
z_test_result <- z.test(
  x = heart_disease$chol,
  y = no_heart_disease$chol,
  alternative = "two.sided",
  mu = 0,
  sigma.x = sd(heart_disease$chol),
  sigma.y = sd(no_heart_disease$chol)
)

# Display the z-test result
z_test_result

# Calculate the standard deviations of cholesterol levels in both groups
sd_heart_disease <- sd(heart_disease$chol)
sd_no_heart_disease <- sd(no_heart_disease$chol)

# Calculate the mean, median, and mode of cholesterol levels in the entire dataset
mean_chol <- mean(heart_data$chol)
median_chol <- median(heart_data$chol)

# Calculate the mode of the 'Condition' variable using the modeest library
library(modeest)
mode_condition <- mfv(heart_data$Condition)