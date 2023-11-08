# Load the tidyverse library
library(tidyverse)

# Display the first few rows of the 'cars' dataset
head(cars)

# Summarize the 'cars' dataset
summary(cars)

# Display the structure of the 'cars' dataset
str(cars)

# Build a linear regression model on the 'cars' dataset
linearMod <- lm(cars$dist ~ cars$speed, data=cars)

# Print the linear regression model
print(linearMod)

# Build a linear regression model on the 'titanic' dataset
linearMod <- lm(titanic$age ~ titanic$fare, data=titanic)

# Print the linear regression model
print(linearMod)

# Calculate the value of 'dist' using the linear regression equation Y = AX + B
d = (3.932 * 8) - 17.579
print(d)

# Summarize the linear regression model
summary(linearMod)

# Calculate the correlation between 'speed' and 'dist' in the 'cars' dataset
correlation <- cor(cars$speed, cars$dist)

# Print the correlation value
correlation
