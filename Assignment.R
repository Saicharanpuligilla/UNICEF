library(ggplot2)
library(dplyr)
library(zoo)

unicef_indicator_2 <- read.csv("D:/users/unicef_indicator_2.csv")
unicef_metadata <- read.csv("D:/users/unicef_metadata.csv")

str(unicef_indicator_2)
str(unicef_metadata)

summary(unicef_indicator_2)
summary(unicef_metadata)

# Droping null values
unicef_metadata_clean <- na.omit(unicef_metadata)

# Replacing missing values with the mean
unicef_metadata_clean <- na.aggregate(unicef_metadata_clean, FUN = mean)
anyNA(unicef_metadata_clean)

str(unicef_metadata_clean)
summary(unicef_metadata_clean)
column_names <- names(unicef_metadata_clean)
column_names
column_names <- names(unicef_metadata)
column_names


# Identifing common columns between the datasets
common_cols <- intersect(names(unicef_metadata_clean), names(unicef_indicator_2))

# Merging the datasets based on common columns
merged_data <- merge(unicef_metadata_clean, unicef_indicator_2, by = common_cols)

# Checking the structure of the merged dataset
str(merged_data)


# Creating a bar chart for life expectancy with data levels
life_expectancy_chart <- ggplot(merged_data, aes(x = factor(Life.expectancy.at.birth..total..years.))) +
  geom_bar(fill = "lightblue") +
  labs(title = "Life Expectancy at Birth",
       x = "Life Expectancy (Years)",
       y = "Count")

# Display the bar chart
print(life_expectancy_chart)


## Scatterplot with a linear regression line
# Computing linear regression
lm_model <- lm(Life.expectancy.at.birth..total..years. ~ obs_value, data = merged_data)

# Extracting slope and intercept from the linear regression model
slope <- lm_model$coefficients[2]
intercept <- lm_model$coefficients[1]

# Creating a scatterplot with a linear regression line using only obs_value
scatterplot_obs_value <- ggplot(merged_data, aes(x = obs_value, y = Life.expectancy.at.birth..total..years.)) +
  geom_point(color = "blue") +  # Scatterplot
  geom_abline(intercept = intercept, slope = slope, color = "red") +  # Linear regression line
  labs(title = "Scatterplot with Linear Regression (obs_value)",
       x = "Observation Value",
       y = "Life Expectancy (Years)")

# Display the scatterplot
print(scatterplot_obs_value)
# Compute linear regression
lm_model <- lm(Life.expectancy.at.birth..total..years. ~ obs_value, data = merged_data)
# Print the summary of the linear regression model
summary(lm_model)


merged_data$time_period <- as.Date(merged_data$time_period, format = "%Y")
# Creating a time-series chart
time_series_chart <- ggplot(merged_data, aes(x = time_period, y = obs_value, label = round(obs_value, 2))) +
  geom_line() +
  geom_text(hjust = -0.1, vjust = 0.5, size = 3, color = "blue") +  # Add data values
  labs(title = "Time Series of obs_value",
       x = "Time Period",
       y = "Observation Value")
# Display the time-series chart
print(time_series_chart)
merged_data



install.packages("maps")
library(maps)
library(ggplot2)



# Converting GDP.per.capita..constant.2015.US.. to numeric
merged_data$GDP.per.capita..constant.2015.US.. <- as.numeric(merged_data$GDP.per.capita..constant.2015.US..)

# Filtering merged_data for countries with GDP per capita greater than 8000
high_gdp_countries <- merged_data[merged_data$GDP.per.capita..constant.2015.US. > 8000, ]

# Retrieving world map data
world_map <- map_data("world")

# Filtering world map data for matched countries
matched_map_data <- subset(world_map, region %in% high_gdp_countries$country)

# Converting GDP per capita to a factor
matched_map_data$GDP.per.capita.factor <- as.factor(matched_map_data$region)

# Creating a world map chart
world_map_chart <- ggplot(matched_map_data, aes(x = long, y = lat, group = group, fill = GDP.per.capita.factor)) +
  geom_polygon(color = "black") +
  scale_fill_discrete(name = "GDP per capita") +
  labs(title = "World Map with GDP per Capita > 8000") +
  theme_void()

# Displaying the world map chart
print(world_map_chart)



