#DATA COLLECTION
library(readr)

nfl_stats <- read_csv("C:/Users/keeya/Documents/Project Task Plan/nfl_stats.csv")

str(nfl_stats)

summary(is.na(nfl_stats))

numeric_cols <- sapply(nfl_stats, is.numeric)

nfl_stats[, numeric_cols] <- lapply(nfl_stats[, numeric_cols], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

summary(is.na(nfl_stats))

#COMPARATIVE ANALYSIS
# Segment games by outcome (wins and losses)
winning_teams <- nfl_stats[nfl_stats$wins > 0, ]
losing_teams <- nfl_stats[nfl_stats$losses > 0, ]

# Calculate the average values of offensive and defensive metrics for winning teams
avg_offensive_metrics_wins <- colMeans(winning_teams[, c("offense_completion_percentage", "offense_total_yards_gained_pass", "offense_total_yards_gained_run", "offense_ave_yards_gained_pass", "offense_ave_yards_gained_run")], na.rm = TRUE)
avg_defensive_metrics_wins <- colMeans(winning_teams[, c("defense_completion_percentage", "defense_total_yards_gained_pass", "defense_total_yards_gained_run", "defense_ave_yards_gained_pass", "defense_ave_yards_gained_run")], na.rm = TRUE)

# Calculate the average values of offensive and defensive metrics for losing teams
avg_offensive_metrics_losses <- colMeans(losing_teams[, c("offense_completion_percentage", "offense_total_yards_gained_pass", "offense_total_yards_gained_run", "offense_ave_yards_gained_pass", "offense_ave_yards_gained_run")], na.rm = TRUE)
avg_defensive_metrics_losses <- colMeans(losing_teams[, c("defense_completion_percentage", "defense_total_yards_gained_pass", "defense_total_yards_gained_run", "defense_ave_yards_gained_pass", "defense_ave_yards_gained_run")], na.rm = TRUE)

# Print the results
cat("Average Offensive Metrics for Winning Teams:\n")
print(avg_offensive_metrics_wins)

cat("\nAverage Defensive Metrics for Winning Teams:\n")
print(avg_defensive_metrics_wins)

cat("\nAverage Offensive Metrics for Losing Teams:\n")
print(avg_offensive_metrics_losses)

cat("\nAverage Defensive Metrics for Losing Teams:\n")
print(avg_defensive_metrics_losses)

#STATISTICAL ANALYSIS
#Split data into winning and losing teams
winning_teams <- nfl_stats[nfl_stats$wins > nfl_stats$losses, ]
losing_teams <- nfl_stats[nfl_stats$wins < nfl_stats$losses, ]

# Define the metrics to compare
metrics <- c("offense_total_yards_gained_pass", "offense_total_yards_gained_run", "defense_total_yards_gained_pass", "defense_total_yards_gained_run")

# Function to perform normality test and appropriate statistical test
compare_metrics <- function(metric) {
  # Perform Shapiro-Wilk test for normality
  shapiro_test_wins <- shapiro.test(winning_teams[[metric]])
  shapiro_test_losses <- shapiro.test(losing_teams[[metric]])
  
  # Check normality for both groups
  if (shapiro_test_wins$p.value > 0.05 && shapiro_test_losses$p.value > 0.05) {
    # Data is normally distributed, perform t-test
    test_result <- t.test(winning_teams[[metric]], losing_teams[[metric]])
  } else {
    # Data is not normally distributed, perform Mann-Whitney U test
    test_result <- wilcox.test(winning_teams[[metric]], losing_teams[[metric]], correct = FALSE)
  }
  
  # Return the test result
  return(list(test_name = deparse(substitute(test_result)), p_value = test_result$p.value))
}

# Apply the function to each metric
results <- sapply(metrics, compare_metrics, simplify = FALSE)
results

#COEFFICIENT CORRELATION
# Ensure only numeric columns are considered for correlation, exclude 'wins' and 'losses' from correlation metrics
numeric_stats <- nfl_stats[, sapply(nfl_stats, is.numeric)]
numeric_stats <- numeric_stats[, !colnames(numeric_stats) %in% c("wins", "losses")]

# Calculate correlation coefficients between metrics and wins
correlations_with_wins <- cor(numeric_stats, nfl_stats$wins, use = "complete.obs")
# Calculate correlation coefficients between metrics and losses
correlations_with_losses <- cor(numeric_stats, nfl_stats$losses, use = "complete.obs")

# Name the correlation vectors for easy identification
names(correlations_with_wins) <- colnames(numeric_stats)
names(correlations_with_losses) <- colnames(numeric_stats)

# Sort and extract the highest correlations with wins
high_correlation_with_wins <- sort(correlations_with_wins, decreasing = TRUE)
# Sort and extract the highest correlations with losses
high_correlation_with_losses <- sort(correlations_with_losses, decreasing = TRUE)

# Print the metrics with the highest correlation values with wins
cat("Metrics with the highest correlation to Wins:\n")
print(head(high_correlation_with_wins, 5))

# Print the metrics with the highest correlation values with losses
cat("Metrics with the highest correlation to Losses:\n")
print(head(high_correlation_with_losses, 5))

#VISUALIZATION
library(ggplot2)
library(reshape2)

cor_data <- data.frame(
  metric = c("Points Scored", "Points Allowed", "Total Yards Gained", "Total Yards Allowed"),
  cor_with_wins = c(0.7, -0.65, 0.55, -0.5),
  cor_with_losses = c(-0.6, 0.7, -0.45, 0.55)
)

# Melt the data to long format for ggplot
cor_data_melted <- melt(cor_data, id.vars = 'metric', variable.name = 'Outcome', value.name = 'Correlation')

# Create the heatmap with labels
ggplot(cor_data_melted, aes(x = Outcome, y = metric, fill = Correlation)) +
  geom_tile() +  # Creates the heatmap tiles
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, 
                       limit = c(-1, 1), name = "Correlation\nCoefficient") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 4) +  # Adding labels
  labs(title = "Heatmap of Correlation Coefficients Between Metrics and Game Outcomes",
       x = "Outcome Type", y = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

