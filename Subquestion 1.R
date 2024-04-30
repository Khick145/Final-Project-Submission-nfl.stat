#DATA
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

# Read the NFL stats data
nfl_data <- read_csv("C:/Users/keeya/Documents/Project Task Plan/nfl_stats.csv")

# Display the structure of the dataframe
str(nfl_data)

# Filter the dataframe to include only data for the Miami Dolphins
miami_dolphins_data <- nfl_data %>%
  filter(team == "MIA")

# Display the structure of the Miami Dolphins dataframe
str(miami_dolphins_data)

# Filter the dataset to include only defensive metrics for the Miami Dolphins
miami_dolphins_defensive_data <- miami_dolphins_data %>%
  select(matches("^defense_"))

# Display the structure of the defensive data
str(miami_dolphins_defensive_data)

# Calculate summary statistics (mean, median, standard deviation) for each defensive metric
summary_stats <- sapply(miami_dolphins_defensive_data, function(x) c(mean(x, na.rm = TRUE), median(x, na.rm = TRUE), sd(x, na.rm = TRUE)))

# Convert summary statistics to a data frame
summary_stats_df <- as.data.frame(summary_stats)
rownames(summary_stats_df) <- c("Mean", "Median", "Standard Deviation")
colnames(summary_stats_df) <- gsub("^defense_", "", colnames(summary_stats_df))

# Calculate mean values for each defensive metric
mean_values <- apply(miami_dolphins_defensive_data, 2, mean, na.rm = TRUE)

# Create a dataframe with mean values and corresponding metrics
mean_df <- data.frame(metric = names(mean_values), mean_value = mean_values, row.names = NULL)

# Rank the defensive metrics based on their mean values
ranked_metrics <- mean_df[order(mean_df$mean_value, decreasing = TRUE), ]

# Print summary statistics and ranked metrics
print(summary_stats_df)
print(ranked_metrics)

# VISUALIZATION
miami_dolphins_data$season <- as.factor(miami_dolphins_data$season)

# Filter only defensive metrics
miami_dolphins_defensive_data <- miami_dolphins_data %>%
  select(matches("^defense_"), season)

# Melt the defensive data for easier plotting
melted_defensive_data <- melt(miami_dolphins_defensive_data, id.vars = "season")

# Create a bar chart for each defensive metric over time
ggplot(melted_defensive_data, aes(x = season, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Miami Dolphins Defensive Metrics Over Time",
       x = "Season",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_discrete(name = "Defensive Metric")

#TREND ANALYSIS
# Filter the dataset to include only the weakest defensive metrics
weakest_metrics <- c("total_yards_gained_pass", "total_yards_gained_run")
weakest_data <- miami_dolphins_data %>%
  select(season, contains(weakest_metrics))

# Melt the data for easier plotting
melted_data <- reshape2::melt(weakest_data, id.vars = "season")

# Create line plots for each weakest defensive metric over time
ggplot(melted_data, aes(x = season, y = value, color = variable, group = variable)) +
  geom_line() +
  labs(title = "Trend Analysis of Weakest Defensive Metrics Over Time",
       x = "Season",
       y = "Value",
       color = "Defensive Metric") +
  theme_minimal() +
  theme(legend.position = "right")

# Summarize the line plots for weakest defensive metrics over time
summary_line_plots <- "The line plots display the trends of the weakest defensive metrics over time for the Miami Dolphins. Each line represents a defensive metric, and the x-axis indicates the season while the y-axis represents the value of the metric. The plot shows how these defensive metrics have changed over multiple seasons, allowing for visual comparison and trend analysis."

# Print the summary
print(summary_line_plots)

