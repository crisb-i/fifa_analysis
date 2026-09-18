# Current FIFA rankings + full 32-team tournament simulation
# Includes shootouts in tied group and knockout matches, and a saved probability chart.

matches <- read.csv("archive/international_matches.csv", stringsAsFactors = FALSE)
matches$date <- as.Date(matches$date)

matches <- subset(
  matches,
  !is.na(home_team_fifa_rank) & !is.na(away_team_fifa_rank) &
    !is.na(home_team_total_fifa_points) & !is.na(away_team_total_fifa_points)
)

home_df <- matches[, c("home_team", "date", "home_team_fifa_rank", "home_team_total_fifa_points")]
away_df <- matches[, c("away_team", "date", "away_team_fifa_rank", "away_team_total_fifa_points")]
names(home_df) <- c("team", "date", "rank", "points")
names(away_df) <- c("team", "date", "rank", "points")
team_rank_data <- rbind(home_df, away_df)
team_rank_data <- subset(team_rank_data, !is.na(rank) & !is.na(points))
team_rank_data <- team_rank_data[order(team_rank_data$date, decreasing = TRUE), ]
team_rank_data <- team_rank_data[!duplicated(team_rank_data$team), ]
team_snapshot <- team_rank_data[order(team_rank_data$rank), ]
team_snapshot <- team_snapshot[1:32, ]
team_snapshot$strength <- 1000 + (32 - team_snapshot$rank) * 12 + team_snapshot$points / 10
ratings <- setNames(team_snapshot$strength, team_snapshot$team)

win_prob <- function(team_a, team_b) {
  sa <- ratings[[team_a]]
  sb <- ratings[[team_b]]
  plogis((sa - sb) / 65)
}

play_match <- function(team_a, team_b, shootout = TRUE) {
  p_a <- win_prob(team_a, team_b)
  g1 <- rpois(1, 1.25 + 0.8 * p_a)
  g2 <- rpois(1, 1.05 + 0.8 * (1 - p_a))

  if (g1 == g2 && shootout) {
    if (runif(1) < p_a) {
      return(team_a)
    } else {
      return(team_b)
    }
  }

  if (g1 > g2) {
    return(team_a)
  } else {
    return(team_b)
  }
}

simulate_group <- function(group_teams) {
  standings <- data.frame(
    team = group_teams,
    played = 0,
    wins = 0,
    draws = 0,
    losses = 0,
    gf = 0,
    ga = 0,
    gd = 0,
    pts = 0,
    stringsAsFactors = FALSE
  )

  for (i in 1:3) {
    for (j in (i + 1):4) {
      a <- group_teams[i]
      b <- group_teams[j]
      p_a <- win_prob(a, b)
      g1 <- rpois(1, 1.25 + 0.8 * p_a)
      g2 <- rpois(1, 1.05 + 0.8 * (1 - p_a))
      winner <- if (g1 == g2) {
        if (runif(1) < p_a) a else b
      } else if (g1 > g2) {
        a
      } else {
        b
      }

      standings$played[standings$team == a] <- standings$played[standings$team == a] + 1
      standings$played[standings$team == b] <- standings$played[standings$team == b] + 1
      standings$gf[standings$team == a] <- standings$gf[standings$team == a] + g1
      standings$gf[standings$team == b] <- standings$gf[standings$team == b] + g2
      standings$ga[standings$team == a] <- standings$ga[standings$team == a] + g2
      standings$ga[standings$team == b] <- standings$ga[standings$team == b] + g1

      if (g1 > g2) {
        standings$wins[standings$team == a] <- standings$wins[standings$team == a] + 1
        standings$losses[standings$team == b] <- standings$losses[standings$team == b] + 1
        standings$pts[standings$team == a] <- standings$pts[standings$team == a] + 3
      } else if (g2 > g1) {
        standings$wins[standings$team == b] <- standings$wins[standings$team == b] + 1
        standings$losses[standings$team == a] <- standings$losses[standings$team == a] + 1
        standings$pts[standings$team == b] <- standings$pts[standings$team == b] + 3
      } else {
        standings$draws[standings$team == a] <- standings$draws[standings$team == a] + 1
        standings$draws[standings$team == b] <- standings$draws[standings$team == b] + 1
        standings$pts[standings$team == a] <- standings$pts[standings$team == a] + 1
        standings$pts[standings$team == b] <- standings$pts[standings$team == b] + 1
      }
    }
  }

  standings$gd <- standings$gf - standings$ga
  standings <- standings[order(-standings$pts, -standings$gd, -standings$gf, standings$team), ]
  as.character(standings$team[1:2])
}

simulate_knockout <- function(team_a, team_b) {
  p_a <- win_prob(team_a, team_b)
  g1 <- rpois(1, 1.2 + 0.7 * p_a)
  g2 <- rpois(1, 1.1 + 0.7 * (1 - p_a))
  if (g1 == g2) {
    if (runif(1) < p_a) team_a else team_b
  } else if (g1 > g2) {
    team_a
  } else {
    team_b
  }
}

simulate_tournament <- function() {
  all_teams <- sample(team_snapshot$team)
  groups <- split(all_teams, rep(1:8, each = 4))
  finalists <- unlist(lapply(groups, simulate_group))

  while (length(finalists) > 1) {
    next_round <- c()
    for (i in seq(1, length(finalists), 2)) {
      next_round <- c(next_round, simulate_knockout(finalists[i], finalists[i + 1]))
    }
    finalists <- next_round
  }

  finalists[1]
}

set.seed(2026)
num_sims <- 1500
winner_counts <- setNames(rep(0, nrow(team_snapshot)), team_snapshot$team)

for (s in 1:num_sims) {
  winner <- simulate_tournament()
  winner_counts[winner] <- winner_counts[winner] + 1
}

result <- data.frame(
  team = team_snapshot$team,
  current_rank = team_snapshot$rank,
  current_points = team_snapshot$points,
  win_probability_pct = round((winner_counts / num_sims) * 100, 3),
  stringsAsFactors = FALSE
)
result <- result[order(-result$win_probability_pct), ]

png("world_cup_probabilities.png", width = 1600, height = 900, res = 220)
par(mar = c(10, 4, 4, 2))
barplot(
  result$win_probability_pct[1:10],
  names.arg = result$team[1:10],
  las = 2,
  col = rep("dodgerblue", 10),
  border = "navy",
  main = "World Cup win probabilities",
  ylab = "Win probability (%)",
  ylim = c(0, 16)
)
dev.off()

cat("Current FIFA ranking snapshot (top 10):\n")
print(head(team_snapshot[, c("team", "rank", "points", "strength")], 10))

cat("\n32-team tournament win probability (top 10):\n")
print(head(result[, c("team", "current_rank", "current_points", "win_probability_pct")], 10))

argentina <- subset(result, team == "Argentina")
peru <- subset(result, team == "Peru")

cat("\nArgentina estimated win probability:", argentina$win_probability_pct[1], "%\n")
if (nrow(peru) > 0) {
  cat("Peru estimated win probability:", peru$win_probability_pct[1], "%\n")
} else {
  cat("Peru not found in the current top-32 rankings snapshot.\n")
}
cat("\nChart saved to: world_cup_probabilities.png\n")
