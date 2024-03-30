# Load necessary packages
library(tidyverse)
library(psych)

# Set working directory
setwd("~/R Projects/archive (5)")

# Read the dataset
TMDB_data <- read.csv("TMDB_movie_dataset_v11.csv")

# View the first 10 records
head(TMDB_data, 10)

# Structure of the dataset
str(TMDB_data)

# Handling missing values
missing_values <- colSums(is.na(TMDB_data))
tibble(Variables = names(missing_values), missing_values = missing_values)

# Handling outliers
numeric_variable <- TMDB_data %>%
  select_if(is.numeric)
describe(numeric_variable)

# Identify and correct outliers
Outliers_values <- TMDB_data %>%
  filter(title %in% c("Adventures in Bora Bora", "The Bear") | title == "Untitled" | original_title == "Untitled" | status == "Canceled" | status == "Rumored" | status == "Planned" | revenue < 0 | runtime < 0)

# Remove outliers
TMDB_data <- TMDB_data %>%
  filter(!revenue < 0 & !title %in% c("Adventures in Bora Bora", "The Bear") & !runtime < 0 & !title == "Untitled" & !status %in% c("Canceled", "Rumored", "Planned"))

# Data transformation
TMDB_data_pre <- TMDB_data %>%
  select(-release_date) %>%
  mutate_if(is.character, as.factor)

# Summary of transformed variables
summary(TMDB_data_pre)

# Select variables and filter data since 2018
TMDB_data_pre$release_date <- TMDB_data$release_date
TMDB_data <- TMDB_data_pre %>%
  select(id, title, release_date, status, runtime,popularity, vote_average, vote_count, revenue, budget, production_companies, genres, original_language, adult) %>%
  filter(release_date > 2018)

# Date transformation and additional variables
TMDB_data$release_date <- as.Date(TMDB_data$release_date, format =  "%Y-%m-%d")
TMDB_data$year <- year(TMDB_data$release_date)
TMDB_data$month <- month(TMDB_data$release_date, label = TRUE)

# Filtering and sorting top-rated movies
TMDB_data <- TMDB_data %>%
  filter(runtime > 60) %>% # to exclude short films
  filter(budget > 100000) %>% # to exclude the vast majority of low-budget films
  mutate(revenue_in_million = round(revenue/1000000, 3), budget_in_million = round(budget/1000000,3)) %>%
  filter(vote_average > 7) %>%
  arrange(desc(popularity))

# View structure of the final dataset
str(TMDB_data)

# Data Analysis

# With the necessary data at hand, we delve into an in-depth analysis of top movies, examining each variable meticulously.

## Analysis

# Histogram of total number of top movies released from 2018 to 2024
ggplot(TMDB_data, aes(year))+
  geom_histogram(binwidth = 1, fill = "#67C6E3", color = "black") +
  labs(title = "Total Number of Top Movies Released from 2018 to 2024", x = "Year", y = "Number of Movies")

# Bar plot of total number of top movies released by year and month (2018-2024)
ggplot(TMDB_data, aes(x = month)) +
  geom_bar(fill = "#67C6E3") +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.01)) +
  facet_wrap(~year) +
  labs(title = "Total Number of Top Movies Released by Year and Month (2018-2024)", x = "Month", y = "Number of Movies")

# Histogram of movie runtime distribution
ggplot(TMDB_data, aes(runtime)) +
  geom_histogram(fill = "#67C6E3") + 
  labs(title = "Movie Runtime Distribution", x = "Runtime (in minutes)", y = "Count")

# Observation of movies with runtime > 400 minutes
TMDB_data %>%
  filter(runtime > 400)

# Histogram of movie popularity distribution
ggplot(TMDB_data, aes(popularity)) +
  geom_histogram(fill = "#67C6E3") +
  labs(title = "Movie Popularity Distribution", x = "Popularity", y = "Count")

# Observation of movies with popularity > 2000
TMDB_data %>%
  filter(popularity > 2000)

# Histogram of movie vote average distribution
ggplot(TMDB_data, aes(vote_average)) +
  geom_histogram(fill = "#67C6E3") +
  labs(title = "Movie Vote Average Distribution", x = "Vote Average", y = "Count")

# Histogram of movie vote distribution
ggplot(TMDB_data, aes(vote_count)) +
  geom_histogram(fill = "#67C6E3") +
  xlim(0,27714) +
  ylim(0,100) +
  labs(title = "Movie Vote Distribution", x = "Vote", y = "Count")

# Observation of movies with vote_count > 20000
TMDB_data %>%
  filter(vote_count > 20000)

# Histogram of movie revenue distribution in million
ggplot(TMDB_data, aes(revenue_in_million)) +
  geom_histogram(fill = "#67C6E3") +
  labs(title = "Movie Revenue Distribution in Million", x = "Revenue (in million)", y = "Count")

# Histogram of movie budget distribution in million
ggplot(TMDB_data, aes(budget_in_million)) +
  geom_histogram(fill = "#67C6E3") +
  labs(title = "Movie Budget Distribution in Million", x = "Budget (in million)", y = "Count")

# Observation of movies with budget_in_million > 300
TMDB_data %>%
  filter(budget_in_million > 300)

# Calculate earnings and losses for movies
TMDB_data_earnings_losses <- TMDB_data %>%
  filter(revenue_in_million > 0) %>%
  mutate(earnings_and_losses = revenue_in_million - budget_in_million)

# Categorize movies into earnings and losses
TMDB_data_earnings_losses <- TMDB_data_earnings_losses %>%
  mutate(earn_of_lose = ifelse(earnings_and_losses >= 0, "Earnings", "Losses"), year = year(release_date))

# Calculate average gains and losses by year
TMDB_data_average_gains_and_losses <- TMDB_data_earnings_losses %>%
  group_by(year, earn_of_lose) %>%
  summarize(mean_earn_lose = round(mean(earnings_and_losses), 3))

# Stacked bar chart of average gains and losses by year (2018-2024)
ggplot(TMDB_data_average_gains_and_losses, aes(year, mean_earn_lose, fill = earn_of_lose)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Earnings" = "#67C6E3", "Losses" = "#FF6969")) +
  geom_text(aes(label = mean_earn_lose, y = ifelse(earn_of_lose == "Earnings", mean_earn_lose * 1.05, mean_earn_lose * 1.4)), color = "black") +
  labs(title = "Stacked Bar Chart of Average Gains and Losses by Year (2018-2024)", x = "", y = "Mean (in million)", fill = "Earnings/Losses")

# Top 3 languages by average revenue
TOP3_language <- TMDB_data_earnings_losses %>%
  filter(!original_language == "cn") %>%
  group_by(original_language) %>%
  summarize(mean_earnings = mean(earnings_and_losses)) %>%
  arrange(desc(mean_earnings)) %>%
  head(3)

# Bar plot of top 3 languages by average revenue
ggplot(TOP3_language, aes(reorder(original_language, mean_earnings), mean_earnings)) +
  geom_bar(stat = "identity", fill = "#67C6E3") +
  coord_flip() +
  labs(title = "Top 3 Languages by Average Revenue", x = "", y = "Mean Revenue (in million)")

