library('tidyverse')

# Read the dataset and get a high-level understanding
data <- read.csv('../input/bike-buyers/bike_buyers.csv', header = TRUE)
head(data)
str(data)

# Delete duplicate rows using unique()
bike_buyers <- unique(data)

# Check for missing values
colSums(is.na(bike_buyers))

# Find rows with missing 'Gender' values
sum(bike_buyers$Gender == "")

# Check the frequency of 'Marital.Status' categories
table(bike_buyers$Marital.Status)

# Assign factors to string values
bike_buyers$Marital.Status <- as.factor(bike_buyers$Marital.Status)
bike_buyers$Gender <- as.factor(bike_buyers$Gender)
bike_buyers$Home.Owner <- as.factor(bike_buyers$Home.Owner)
bike_buyers$Purchased.Bike <- as.factor(bike_buyers$Purchased.Bike)

# Summary statistics and check for missing values again
str(bike_buyers)
summary(bike_buyers)
colSums(is.na(bike_buyers))

# Replace missing values in 'Income' and 'Age' with medians
median_income <- median(na.omit(bike_buyers$Income))
median_age <- median(na.omit(bike_buyers$Age))
bike_buyers_clean <- bike_buyers
bike_buyers_clean$Income[is.na(bike_buyers_clean$Income)] <- median_income
bike_buyers_clean$Age[is.na(bike_buyers_clean$Age)] <- median_age

# Mode function
get_mode <- function(x) {
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

# Replace missing values in 'Marital.Status', 'Gender', and 'Children' with modes
bike_buyers_clean$Marital.Status[is.na(bike_buyers_clean$Marital.Status)] <- get_mode(bike_buyers$Marital.Status)
bike_buyers_clean$Gender[is.na(bike_buyers_clean$Gender)] <- get_mode(bike_buyers$Gender)
bike_buyers_clean$Children[is.na(bike_buyers_clean$Children)] <- get_mode(bike_buyers$Children)
bike_buyers_clean$Home.Owner[is.na(bike_buyers_clean$Home.Owner)] <- get_mode(bike_buyers$Home.Owner)

# Replace missing values in 'Cars' with the mean
bike_buyers_clean$Cars[is.na(bike_buyers_clean$Cars)] <- mean(bike_buyers$Cars, na.rm = TRUE)

# Save the cleaned dataset
write.csv(bike_buyers_clean, "bike_buyers_clean.csv", quote = FALSE, row.names = TRUE)


# Box plot of 'Income'
boxplot(bike_buyers$Income, main = 'Income Boxplot')

#Outliers
OutVals = boxplot(bike_buyers$Income)$out
print(OutVals)

which(bike_buyers$Income %in% OutVals)

x = bike_buyers_clean$Income[!(bike_buyers_clean$Income %in% OutVals) ]
boxplot(bike_buyers_clean$Income)