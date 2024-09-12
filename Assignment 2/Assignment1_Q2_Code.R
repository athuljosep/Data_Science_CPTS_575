#Question 2


# Reading the dataset using R
bikes_data <- read.csv("bikes.csv")

# Summary statistics for quantitative variables
quantitative_values <- bikes_data %>%
  summarise(
    temp_range = max(temp) - min(temp),
    temp_mean = mean(temp, na.rm = TRUE),
    temp_standarddeviation = sd(temp, na.rm = TRUE),
    
    atemp_range = max(atemp) - min(atemp),
    atemp_mean = mean(atemp, na.rm = TRUE),
    atemp_standarddeviation= sd(atemp, na.rm = TRUE),
    
    humidity_range = max(humidity) - min(humidity),
    humidity_mean = mean(humidity, na.rm = TRUE),
    humidity_standarddeviation = sd(humidity, na.rm = TRUE),
    
    windspeed_range = max(windspeed) - min(windspeed),
    windspeed_mean = mean(windspeed, na.rm = TRUE),
    windspeed_standarddeviation = sd(windspeed, na.rm = TRUE),
    
    count_range = max(count) - min(count),
    count_mean = mean(count, na.rm = TRUE),
    count_standarddeviation = sd(count, na.rm = TRUE)
  )

print(quantitative_values)

# Season having the highest average bike rental count
season_average <- bikes_data %>%
  group_by(season) %>%
  summarise(count_mean = mean(count, na.rm = TRUE)) %>%
  arrange(desc(count_mean))

# Print the average of every seson
print(season_average)

# Extract the season with the highest average bike rental count
Season_with_highest_average_count <- season_average %>%
  slice(1)

# Print the season with the highest average count
print(Season_with_highest_average_count)


# Boxplot of bike rental counts per month
ggplot(bikes_data, aes(x = factor(month), y = count)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Bike Rental Counts on basis of Weather Condition",
       x = "Condition of the weather",
       y = "Count of Bike Rental")



# Assuming your dataset is loaded into a dataframe called `bikes`
# Convert the date column to Date type if it's not already
bikes_data$date <- as.Date(bikes_data$date)

# Extract the month from the date column
bikes_data$month <- format(bikes_data$date, "%m")

# Create a bar plot showing the count of rentals for each month
library(ggplot2)

ggplot(bikes_data, aes(x = month, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Bike Rentals for Each Month", x = "Month", y = "Count of Rentals") +
  theme_minimal()




# Bar plot showing the total rentals for each month
ggplot(bikes_data, aes(x = month, y = count)) +
  stat_summary(fun = sum, geom = "bar", fill = "pink") +
  labs(title = "Total Bike Rentals by Month",
       x = "Month of the year",
       y = "Total Bike Rental Count")


# Select quantitative variables
quantitative_values <- bikes_data %>% 
  select(temp, atemp, humidity, windspeed, count)

# Create a correlation matrix
correlation_matrix <- cor(quantitative_values, use = "complete.obs")
print(correlation_matrix)

# Visualize the correlation matrix using ggplot
library(reshape2)
correlation_data <- melt(correlation_matrix)

ggplot(correlation_data, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix of Quantitative Variables",
       x = "Variables",
       y = "Variables")











