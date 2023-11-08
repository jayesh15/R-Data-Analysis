# Load the tidyverse library
library(tidyverse)

# Read the 'omplyic' dataset
om <- read.csv("/kaggle/input/omplyic/F.csv")

# Display the column names of the dataset
names(om)

# Display the first 10 rows of the dataset
head(om, 10)

# Convert the dataset to a tibble
tibble::as_tibble(om)

# Display the structure of the dataset
glimpse(om)

# Display the structure of the dataset
str(om)

# Filter the dataset for rows where CountryCode is "AUS", Medal is "Gold", and Sex is "Women"
om %>% filter(CountryCode == "AUS" & Medal == "Gold" & Sex == "Women")

# Filter the dataset for rows where CountryCode is "AUS" and Medal is either "Gold" or "Silver"
om %>% filter(CountryCode == "AUS" & (Medal == "Gold" | Medal == "Silver"))

# Print the names of events where the country "AUS" has won gold medals
om %>% filter(CountryCode == "AUS" & Medal == "Gold") %>% select(Sport, Event, 'Country' = CountryCode)

# Arrange the dataset in descending order of ResultInSeconds, filter for "AUS," and select specific columns
om %>% arrange(desc(ResultInSeconds)) %>% filter(CountryCode == "AUS" & (Medal == "Gold" | Medal == "Silver")) %>% select(CountryCode, Sport, Medal, ResultInSeconds)

# Filter the dataset for "AUS," where Medal is either "Gold" or "Silver," and select specific columns
om %>% filter(CountryCode == "AUS" & (Medal == "Gold" | Medal == "Silver")) %>% select(Sport, Medal)

# Select the "GDP.2011" and "pop.2010" columns and create a new column "calcol" by dividing "GDP.2011" by "pop.2010"
om %>% select(GDP.2011, pop.2010) %>% mutate(calcol = GDP.2011 / pop.2010)

# Create new binary columns for Gold, Silver, and Bronze medals using case_when
om <- om %>% mutate(Gold = case_when(Medal == "Gold" ~ 1))
om <- om %>% mutate(Silver = case_when(Medal == "Silver" ~ 1))
om <- om %>% mutate(Bronze = case_when(Medal == "Bronze" ~ 1))

# Display the distinct values in the "Medal" column
om %>% distinct(Medal)

# Replace NA values in the dataset with 0
om <- om %>% replace(is.na(.), 0)

# Count the number of rows where "Gold" is equal to 1
om %>% count(Gold == 1)

# Summarize statistics on the "Gold" column
om %>% summarise(n = n(), sum_gold = sum(Gold), median_gold = median(Gold), mean_gold = mean(Gold), var_gold = var(Gold), sd_gold = sd(Gold))

# Summarize statistics on the "GDP.2011" column
om %>% summarize(n = n(), sumGold = sum(GDP.2011), minGold = min(GDP.2011), maxGold = max(GDP.2011),
  `25th%tile` = quantile(GDP.2011, 0.25), IQRgold = IQR(GDP.2011),
  `75th%tile` = quantile(GDP.2011, 0.75))

# Find the number of different medals won by each country
om %>% group_by(CountryCode) %>% summarize(n = n(), `Total Gold` = sum(Gold), `Total Silver` = sum(Silver), `Total Bronze` = sum(Bronze))

# Rename the "Event" column to "Category"
rename(om, Category = Event)
