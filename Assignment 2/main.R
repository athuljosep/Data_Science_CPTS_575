#libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# reading the data
red_wine_data = read.csv("winequality-red.csv")

# finding the median quality and mean alcohol level
median_quality <- median(red_wine_data$quality)
mean_alcohol_level <- mean(red_wine_data$alcohol)

# generating scatter plot
plot(
  red_wine_data$density,
  red_wine_data$volatile_acidity,
  xlab = "wine density",
  ylab = "volatile acidity",
  main = "Wine Density vs Volatile Acidity",
)

# creating ALevel variable
red_wine_data <- red_wine_data %>%
  mutate(ALevel = ifelse(alcohol > 10.5, "High", "Medium"))

# calculating the sulphates/chlorides
red_wine_data <- red_wine_data %>%
  mutate(sulphates_chlorides = sulphates / chlorides)

# generating box plot
box_plot <- ggplot(red_wine_data, aes(x = ALevel, y = sulphates_chlorides)) +
  geom_boxplot() +
  labs(
    title = "Sulphates/Chlorides vs ALevel",
    x = "ALevel",
    y = "Sulphates / Chlorides"
  ) +
  theme_classic()

print(box_plot)

# samples in high category
high_samples <- red_wine_data %>%
  filter(ALevel == "High") %>%
  nrow()
print(high_samples)



# Histogram for High ALevel (> 10.5)
high_Alevel_plot <- ggplot(filter(red_wine_data, ALevel == "High"), aes(x = citric_acid)) +
  geom_histogram(binwidth = 0.03, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram for High ALevel Wines",
       x = "Citric Acid",
       y = "Number")

# Histogram for Medium ALevel (<= 10.5)
medium_Alevel_plot <- ggplot(filter(red_wine_data, ALevel == "Medium"), aes(x = citric_acid)) +
  geom_histogram(binwidth = 0.03, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram for Medium ALevel Wines",
       x = "Citric Acid",
       y = "Number")

# Display histograms side by side
grid.arrange(high_Alevel_plot, medium_Alevel_plot, ncol = 2)



# Function to create a histogram
create_histogram <- function(data, title, fill_color) {
  ggplot(data, aes(x = citric_acid)) +
    geom_histogram(binwidth = 0.03, fill = fill_color, color = "black", alpha = 0.7) +
    labs(
      title = title,
      x = "Citric Acid",
      y = "Number"
    )
}

# Create histograms for High and Medium ALevel
high_Alevel_plot <- create_histogram(filter(red_wine_data, ALevel == "High"), "Histogram for High ALevel Wines", "red")
medium_Alevel_plot <- create_histogram(filter(red_wine_data, ALevel == "Medium"), "Histogram for Medium ALevel Wines", "blue")

# Display histograms side by side
grid.arrange(high_Alevel_plot, medium_Alevel_plot, ncol = 2)

# Scatterplot: Fixed acidity vs Citric Acid
scat_plot1 <- ggplot(red_wine_data, aes(x = fixed_acidity, y = citric_acid)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(x = "Fixed Acidity",
       y = "Citric Acid",
       title = "Fixed acidity vs Citric Acid")
print(scat_plot1)

# Scatterplot: Total Sulfur Dioxide vs Chlorides
scat_plot2 <- ggplot(red_wine_data, aes(x = total_sulfur_dioxide, y = chlorides)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(x = "Total Sulfur Dioxide",
       y = "Chlorides",
       title = "Total Sulfur Dioxide vs Chlorides")
print(scat_plot2)


# reading the data
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

# Summary statistics for quantitative variables
quantitative_values <- bikes_data %>%
  summarise(across(c(temp, atemp, humidity, windspeed, count), 
                   list(
                     range = ~max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE),
                     mean = ~mean(.x, na.rm = TRUE),
                     standarddeviation = ~sd(.x, na.rm = TRUE)
                   ), .names = "{.col}_{.fn}")
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



# Calculate the average bike rental count for each season and sort in descending order
season_average <- bikes_data %>%
  group_by(season) %>%
  summarise(count_mean = mean(count, na.rm = TRUE)) %>%
  arrange(desc(count_mean))

# Display the average bike rental count for all seasons
print(season_average)

# Display the season with the highest average bike rental count
print(season_average[1, ])

# boxplot to visualize bike rental counts across different weather conditions
ggplot(bikes_data, aes(x = factor(weather), y = count)) +
  geom_boxplot(fill = "red") +  
  labs(
    title = "Bike Rental Counts by Weather Condition", 
    x = "Weather Condition",  
    y = "Number of Bike Rentals"  
  )


# Ensure the 'date' column is converted to Date type and extract the month as a new column
bikes_data$date <- as.Date(bikes_data$date, format = "%m/%d/%y")
bikes_data$month <- format(bikes_data$date, "%B")  # Extract month in full name

# Create a bar plot showing the total number of rentals for each month
ggplot(bikes_data, aes(x = factor(month, levels = month.name), weight = count)) +
  geom_bar(fill = "blue") +  
  labs(title = "Total Bike Rentals by Month",  
       x = "Month",  
       y = "Total Rentals") +  
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Select relevant quantitative variables
quantitative_values <- bikes_data %>%
  select(temp, atemp, humidity, windspeed, count)

# Create a correlation matrix
correlation_matrix <- cor(quantitative_values, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Reshape the correlation matrix for visualization using melt
correlation_data <- melt(correlation_matrix)

# Visualize the correlation matrix using ggplot2
ggplot(correlation_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  
  scale_fill_gradient2(low = "green", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix of Quantitative Variables",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  








# Load necessary libraries
library(ggplot2)
library(dplyr)
library(reshape2)

# Select relevant quantitative variables from the full dataset
quantitative_values <- bikes_data %>% 
  select(temp, atemp, humidity, windspeed, count)

# Create a correlation matrix
correlation_matrix <- cor(quantitative_values, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Reshape the correlation matrix for visualization using melt
correlation_data <- melt(correlation_matrix)

# Visualize the correlation matrix using ggplot2
ggplot(correlation_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Add grid lines for better readability
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix of Quantitative Variables",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +  # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Scatterplot matrix for visualizing relationships between variables
pairs(quantitative_values, main = "Scatterplot Matrix of Quantitative Variables")

