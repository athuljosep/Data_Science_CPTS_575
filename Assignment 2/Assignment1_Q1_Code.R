red_wine_data =  read.csv("winequality-red.csv", header = TRUE)


# Calculate the median of the quality column along with mean of alcohol
quality_median <- median(red_wine_data$quality)
alcohol_mean <- mean(red_wine_data$alcohol)
print(quality_median)
print(alcohol_mean)

plot(
  red_wine_data$density,                      # x-axis: density
  red_wine_data$volatile_acidity,             # y-axis: volatile_acidity
  xlab = "Wine Density",                      # x-axis label
  ylab = "Volatile Acidity",                  # y-axis label
  main= "Wine Density vs Volatile Acidity-SCATTERPLOT", # Title
  pch = 17,                                   # Using cones to represent
  col = "pink"                                # Choosing Pink as my color
)

grid() #Making it cleaner


library(dplyr)
library(ggplot2)

#Creating of the new varuabke called ALevel
red_wine_data <- red_wine_data %>%
  mutate(ALevel = ifelse(alcohol > 10.5, "High", "Medium"))

#calculating the sulphates/chlorides ratio
red_wine_data <- red_wine_data %>%
  mutate(sulphates_chlorides_ratio = sulphates / chlorides)

#Side-by-side Boxplot for specified A level
box_plot1 <- ggplot(red_wine_data, aes(x = ALevel, y = sulphates_chlorides_ratio, fill = ALevel)) +
  geom_boxplot() +labs(title = "Sulphates/Chlorides Ratio by ALevel Boxplot",
       x = "ALevel",
       y = "Sulphates / Chlorides Ratio")

print(box_plot1)


#No of samples in high category
high_count <- red_wine_data %>%
  filter(ALevel == "High") %>%
  nrow()

print(high_count)



# Histogram for High ALevel (> 10.5)
ggplot(filter(red_wine_data, ALevel == "High"), aes(x = citric_acid)) +
  geom_histogram(binwidth = 0.03, fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Histogram for High ALevel Wines",
       x = "Citric Acid",
       y = "Number") 

# Histogram for Medium ALevel (<= 10.5)
ggplot(filter(red_wine_data, ALevel == "Medium"), aes(x = citric_acid)) +
  geom_histogram(binwidth = 0.03, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram for Medium ALevel Wines",
       x = "Citric Acid",
       y = "Number")

# Scatterplot (fixed acidity & pH level)
scat_plot3 <- ggplot(red_wine_data, aes(x = fixed_acidity, y = pH)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(x = "Fixed Acidity",
       y = "pH level",
       title = "Fixed acidity and pH Level")

print(scat_plot3)

# Scatterplot (density & volatile_acidity)
scat_plot2 <- ggplot(red_wine_data, aes(x = density, y = volatile_acidity)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(x = "Density",
       y = "Volatile acidity",
       title = "Wine Density and Volatile acidity")

print(scat_plot2)


