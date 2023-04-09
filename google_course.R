
# Download dataset from https://www.kaggle.com/datasets/lashagoch/life-expectancy-who-updated

library(readr)
library(ggplot2)
library(dplyr)


df <- read_csv("archive/Life-Expectancy-Data-Updated.csv")
#View(df)

# Create a new variable that separates Life_expectancy into three classes based on quantiles
df$Life_expectancy_class <- cut(df$Life_expectancy, breaks = quantile(df$Life_expectancy, probs = c(0, 0.33, 0.67, 1)), labels = c("Low", "Medium", "High"))

# Plot a scatter plot of life expectancy against GDP per capita, with different colors for each region
ggplot(df, aes(x = GDP_per_capita, y = Life_expectancy, color = Region)) +
  geom_point(size = 3) + scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "GDP per capita", y = "Life expectancy", color = "Region") +
  ggtitle("Relationship between life expectancy and GDP per capita") +
  labs(subtitle = "Data from WHO, color-coded by region")

# Group the data by region and calculate the average life expectancy for each region
df_region <- df %>%
  group_by(Region) %>%
  summarize(avg_life_expectancy = mean(Life_expectancy, na.rm = TRUE))

# Create a bar plot of the average life expectancy by region
ggplot(df_region, aes(x = Region, y = avg_life_expectancy, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(x = "Region", y = "Average life expectancy", fill = "Region") +
  ggtitle("Average life expectancy by region") + coord_flip()

# Create a box plot of schooling by region
ggplot(df, aes(x = Region, y = Schooling, fill = Region)) +
  geom_boxplot() +
  labs(x = "Region", y = "Schooling", fill = "Region") +
  ggtitle("Distribution of Schooling by Region")
  
# Create a correlation matrix of the variables of interest
corr_matrix <- cor(df[, c(4:20)])

# Plot a heatmap of the correlation matrix
ggplot(melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Matrix of WHO Dataset Variables",
       x = "Variable 1",
       y = "Variable 2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Create a subset of the data with the variables of interest
df_subset <- df[, c("Life_expectancy", "Adult_mortality")]

# Plot a scatter plot with a linear regression line
ggplot(df_subset, aes(x = Adult_mortality, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Life Expectancy and Adult Mortality",
       x = "Adult Mortality",
       y = "Life Expectancy")

