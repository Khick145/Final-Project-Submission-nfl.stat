#DATA
library(readr)
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# Read the CSV file
data <- read_csv("C:/Users/keeya/Documents/Project Task Plan/nfl_stats.csv")

# Prepare data subsets for offensive and defensive metrics
offensive_metrics <- data[c("offense_completion_percentage", "offense_total_yards_gained_pass", "offense_total_yards_gained_run", "offense_n_interceptions", "offense_n_fumbles_lost_pass", "offense_n_fumbles_lost_run", "offense_success_rate_pass", "offense_success_rate_run", "points_scored")]
defensive_metrics <- data[c("defense_completion_percentage", "defense_total_yards_gained_pass", "defense_total_yards_gained_run", "defense_n_interceptions", "defense_n_fumbles_lost_pass", "defense_n_fumbles_lost_run", "defense_success_rate_pass", "defense_success_rate_run", "points_allowed")]

# Calculate correlation matrices for offensive and defensive metrics
correlation_matrix_offense <- cor(offensive_metrics, use = "complete.obs")
correlation_matrix_defense <- cor(defensive_metrics, use = "complete.obs")

# Visualizing the correlation matrices
corrplot(correlation_matrix_offense, method = "circle", type = "upper", title = "Correlation Matrix for Offensive Metrics")
corrplot(correlation_matrix_defense, method = "circle", type = "upper", title = "Correlation Matrix for Defensive Metrics")

# Identify and print strong correlations
strong_correlations_offense <- subset(as.data.frame(as.table(correlation_matrix_offense)), abs(Freq) > 0.5 & Var1 != Var2)
strong_correlations_defense <- subset(as.data.frame(as.table(correlation_matrix_defense)), abs(Freq) > 0.5 & Var1 != Var2)

print("Strong Correlations in Offensive Metrics:")
print(strong_correlations_offense)
print("Strong Correlations in Defensive Metrics:")
print(strong_correlations_defense)

#SCATTERPLOT ANALYSIS
# Scatter plot of Total Offensive Yards Gained vs. Points Scored
ggplot(data, aes(x = offense_total_yards_gained_pass + offense_total_yards_gained_run, y = points_scored)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(x = "Total Offensive Yards Gained (Pass + Run)", y = "Points Scored", title = "Total Yards Gained vs. Points Scored") +
  theme_minimal()

# Scatter plot of Total Defensive Yards Allowed vs. Points Allowed
ggplot(data, aes(x = defense_total_yards_gained_pass + defense_total_yards_gained_run, y = points_allowed)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(x = "Total Defensive Yards Allowed (Pass + Run)", y = "Points Allowed", title = "Total Yards Allowed vs. Points Allowed") +
  theme_minimal()

# Scatter plot of Total Turnovers vs. Points Scored
ggplot(data, aes(x = offense_n_interceptions + offense_n_fumbles_lost_pass + offense_n_fumbles_lost_run, y = points_scored)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(x = "Total Turnovers (Interceptions + Fumbles Lost)", y = "Points Scored", title = "Turnovers vs. Points Scored") +
  theme_minimal()

# Scatter plot of Turnovers Forced vs. Points Allowed
ggplot(data, aes(x = defense_n_interceptions + defense_n_fumbles_lost_pass + defense_n_fumbles_lost_run, y = points_allowed)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(x = "Total Turnovers Forced (Interceptions + Fumbles Lost)", y = "Points Allowed", title = "Turnovers Forced vs. Points Allowed") +
  theme_minimal()

# Scatter plot of Total Yards Allowed (Pass + Run) vs. Points Allowed
ggplot(data, aes(x = defense_total_yards_gained_pass + defense_total_yards_gained_run, y = points_allowed)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(x = "Total Yards Allowed (Pass + Run)", y = "Points Allowed", title = "Scatter Plot of Total Yards Allowed vs. Points Allowed") +
  theme_minimal()

# Scatter plot of Turnovers Forced vs. Points Allowed
ggplot(data, aes(x = defense_n_interceptions + defense_n_fumbles_lost_pass + defense_n_fumbles_lost_run, y = points_allowed)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(x = "Total Turnovers Forced (Interceptions + Fumbles Lost)", y = "Points Allowed", title = "Scatter Plot of Turnovers Forced vs. Points Allowed") +
  theme_minimal()

#GAME OUTCOME
# Check column names first to adjust if needed
print(names(data))

# Group data by game outcome
grouped_data <- data %>%
  group_by(wins, losses, ties) %>%
  summarise(
    avg_offense_yards = mean(offense_total_yards_gained_pass + offense_total_yards_gained_run, na.rm = TRUE),
    avg_defense_yards = mean(defense_total_yards_gained_pass + defense_total_yards_gained_run, na.rm = TRUE),
    avg_points_scored = mean(points_scored, na.rm = TRUE),
    avg_points_allowed = mean(points_allowed, na.rm = TRUE)
  )

# Print the summary statistics for each outcome
print("Summary Statistics by Game Outcome:")
print(grouped_data)

# Visualization of average offensive and defensive yards by outcome
ggplot(grouped_data, aes(x = wins, y = avg_offense_yards, fill = "Wins")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = avg_defense_yards, fill = "Defensive Yards"), stat = "identity", position = "dodge", alpha = 0.7) +
  labs(x = "Game Outcome", y = "Average Yards", title = "Average Yards by Game Outcome") +
  scale_fill_manual(values = c("Wins" = "blue", "Defensive Yards" = "red")) +
  theme_minimal()

# Visualization of average points scored and allowed by outcome
ggplot(grouped_data, aes(x = wins, y = avg_points_scored, fill = "Points Scored")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = avg_points_allowed, fill = "Points Allowed"), stat = "identity", position = "dodge", alpha = 0.7) +
  labs(x = "Game Outcome", y = "Average Points", title = "Average Points by Game Outcome") +
  scale_fill_manual(values = c("Points Scored" = "green", "Points Allowed" = "purple")) +
  theme_minimal()

#SUMMARY
# Check column names
print("Column Names:")
print(names(data))

# Summary of the dataset
print("Summary of the Dataset:")
print(summary(data))

# Structure of the dataset
print("Structure of the Dataset:")
print(str(data))

# Correlation analysis
correlation_matrix <- cor(data[, c("offense_completion_percentage", 
                                   "offense_total_yards_gained_pass", 
                                   "offense_total_yards_gained_run", 
                                   "offense_success_rate_pass", 
                                   "offense_success_rate_run", 
                                   "defense_completion_percentage", 
                                   "defense_total_yards_gained_pass", 
                                   "defense_total_yards_gained_run", 
                                   "defense_success_rate_pass", 
                                   "defense_success_rate_run", 
                                   "points_scored", 
                                   "points_allowed")])

# Print correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)

# Summary of correlation
print("Summary of Correlation:")
print(summary(correlation_matrix))

# Print the entire dataset
print("Complete Dataset:")
print(data)
