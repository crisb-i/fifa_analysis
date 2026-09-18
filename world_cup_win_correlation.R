# Which variables matter most for World Cup success?
# This script tests which variables are most correlated with a team's
# tournament-win likelihood using the FIFA match dataset.

matches <- read.csv("archive/international_matches.csv", stringsAsFactors = FALSE)
matches$date <- as.Date(matches$date)

# Keep rows with valid FIFA ranking and points data
matches <- subset(
  matches,
  !is.na(home_team_fifa_rank) & !is.na(away_team_fifa_rank) &
    !is.na(home_team_total_fifa_points) & !is.na(away_team_total_fifa_points)
)

# Build a per-team dataset using latest available values for each side
home_df <- matches[, c(
  "home_team", "date",
  "home_team_fifa_rank", "home_team_total_fifa_points",
  "home_team_goalkeeper_score",
  "home_team_mean_defense_score",
  "home_team_mean_offense_score",
  "home_team_mean_midfield_score"
)]
away_df <- matches[, c(
  "away_team", "date",
  "away_team_fifa_rank", "away_team_total_fifa_points",
  "away_team_goalkeeper_score",
  "away_team_mean_defense_score",
  "away_team_mean_offense_score",
  "away_team_mean_midfield_score"
)]

names(home_df) <- c("team", "date", "rank", "points", "goalkeeper", "defense", "offense", "midfield")
names(away_df) <- c("team", "date", "rank", "points", "goalkeeper", "defense", "offense", "midfield")

team_data <- rbind(home_df, away_df)
team_data <- subset(team_data, !is.na(rank) & !is.na(points))
team_data <- team_data[order(team_data$date, decreasing = TRUE), ]
team_data <- team_data[!duplicated(team_data$team), ]
team_data <- team_data[order(team_data$rank), ]
team_data <- head(team_data, 32)

# Team strength metric similar to the tournament simulation
team_data$strength <- 1000 + (32 - team_data$rank) * 12 + team_data$points / 10

# Create a continuous proxy for tournament success
team_data$win_probability <- 100 * plogis((team_data$strength - mean(team_data$strength)) / 60)

# Restrict to complete cases for the regression model
model_data <- subset(team_data,
  !is.na(rank) & !is.na(points) & !is.na(goalkeeper) &
    !is.na(defense) & !is.na(offense) & !is.na(midfield)
)

# Correlation with tournament-win likelihood proxy
predictors <- c("rank", "points", "strength", "goalkeeper", "defense", "offense", "midfield")
cor_results <- do.call(rbind, lapply(predictors, function(var_name) {
  x <- model_data[[var_name]]
  ct <- cor.test(x, model_data$win_probability)
  data.frame(
    variable = var_name,
    correlation = as.numeric(ct$estimate),
    p_value = ct$p.value,
    stringsAsFactors = FALSE
  )
}))

cor_results <- cor_results[order(abs(cor_results$correlation), decreasing = TRUE), ]
cor_results$correlation <- round(cor_results$correlation, 3)
cor_results$p_value <- sprintf("%.2e", cor_results$p_value)
cor_results$rank_order <- seq_len(nrow(cor_results))

cor_table <- cor_results[, c("rank_order", "variable", "correlation", "p_value")]
colnames(cor_table) <- c("Rank", "Variable", "Correlation", "P-Value")

cat("\nVariable correlation with simulated World Cup win probability:\n")
print(cor_table, row.names = FALSE)

# Save the table as a PNG image
png("world_cup_correlation_table.png", width = 1600, height = 900, res = 220)
par(mar = c(5, 5, 4, 2))
plot.new()
text(0.5, 0.95, "Correlations to World Cup Wins", cex = 1.2, font = 2)

# Create a table layout in the image
table_x <- 0.05
table_y <- 0.85

text(table_x, table_y, "Rank", pos = 4, cex = 0.9, font = 2)
text(table_x + 0.18, table_y, "Variable", pos = 4, cex = 0.9, font = 2)
text(table_x + 0.52, table_y, "Correlation", pos = 4, cex = 0.9, font = 2)
text(table_x + 0.74, table_y, "P-Value", pos = 4, cex = 0.9, font = 2)

for (i in 1:nrow(cor_table)) {
  y <- table_y - (i * 0.08)
  text(table_x, y, cor_table$Rank[i], pos = 4, cex = 0.8)
  text(table_x + 0.18, y, cor_table$Variable[i], pos = 4, cex = 0.8)
  text(table_x + 0.52, y, format(cor_table$Correlation[i], digits = 3, nsmall = 3), pos = 4, cex = 0.8)
  text(table_x + 0.74, y, cor_table$`P-Value`[i], pos = 4, cex = 0.8)
}

dev.off()

# Linear model to examine statistical significance
model <- lm(
  win_probability ~ rank + points + strength + goalkeeper + defense + offense + midfield,
  data = model_data
)
summary(model)

# Extract the strongest significant coefficient(s)
coef_summary <- summary(model)$coefficients
print(coef_summary)

cat("\nMost correlated predictor:\n")
print(cor_results[1, ])

# A simple visual check
pairs(
  model_data[, c("win_probability", "strength", "points", "offense", "defense", "rank")],
  main = "Relationship between team metrics and win likelihood proxy"
)
