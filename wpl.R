library(jsonlite)
library(dplyr)
library(purrr)

match_ids <- c(
  1513703,1513702,1513701,1513700,1513699,1513698,
  1513697,1513696,1513695,1513694,1513693,1513692,
  1513691,1513690,1513689,1513688,1513687,1513686,
  1513685,1513684,1513683,1513682
)

files <- paste0(match_ids, ".json")

library(jsonlite)
library(dplyr)
library(purrr)

read_match_balls <- function(file) {
  data <- fromJSON(file, simplifyVector = FALSE)
  
  match_id <- tools::file_path_sans_ext(basename(file))
  match_date <- data$info$dates[1]
  
  map_dfr(seq_along(data$innings), function(i) {
    inn <- data$innings[[i]]
    
    map_dfr(inn$overs, function(over) {
      map_dfr(over$deliveries, function(del) {
        tibble(
          match_id = match_id,
          date = match_date,
          batting_team = inn$team,
          bowling_team = setdiff(data$info$teams, inn$team)[1],
          innings = i,
          over = over$over,
          
          batter = del$batter,
          bowler = del$bowler,
          non_striker = del$non_striker,
          
          runs_batter = del$runs$batter,
          runs_extras = del$runs$extras,
          runs_total = del$runs$total,
          
          wide   = as.integer(!is.null(del$extras$wides)),
          noball = as.integer(!is.null(del$extras$noballs)),
          wicket = ifelse(is.null(del$wickets), 0, 1),
          dismissal_kind = ifelse(
            is.null(del$wickets),
            NA_character_,
            del$wickets[[1]]$kind
          )
        )
      })
    })
  })
}

all_balls <- map_dfr(files, read_match_balls) %>%
  mutate(
    illegal = (wide == 1 | noball == 1),
    bowler_wicket = ifelse(wicket == 1 & dismissal_kind != "run out", 1, 0),
    batter_wicket = ifelse(wicket == 1 & dismissal_kind != "run out", 1, 0)
  )
all_balls <- all_balls %>%
  group_by(match_id, innings) %>%
  mutate(
    legal_count = cumsum(!illegal),
    ball_in_innings = lag(legal_count, default = 0) + 1
  ) %>%
  ungroup()

ball_stats <- all_balls %>%
  group_by(ball_in_innings) %>%
  summarise(
    avg_runs = mean(runs_total),
    wicket_prob = mean(wicket),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(ball_in_innings)

runs_smooth <- loess(avg_runs ~ ball_in_innings, data = ball_stats, span = 0.4)
ball_stats$avg_runs_smooth <- predict(runs_smooth)
ggplot(ball_stats, aes(x = ball_in_innings)) +
  geom_line(aes(y = avg_runs), color = "blue", alpha = 0.5) +
  geom_line(aes(y = avg_runs_smooth), color = "red") +
  labs(title = "Average Runs per Ball in Innings",
       x = "Ball in Innings",
       y = "Average Runs") +
  theme_minimal()

wicket_smooth <- loess(wicket_prob ~ ball_in_innings, data = ball_stats, span = 0.4)
ball_stats$wicket_prob_smooth <- predict(wicket_smooth)
ggplot(ball_stats, aes(x = ball_in_innings)) +
  geom_line(aes(y = wicket_prob), color = "blue", alpha = 0.5) +
  geom_line(aes(y = wicket_prob_smooth), color = "red") +
  labs(title = "Wicket Probability per Ball in Innings",
       x = "Ball in Innings",
       y = "Wicket Probability") +
  theme_minimal()

ball_expect <- ball_stats %>%
  transmute(
    ball_in_innings,
    avg_runs = avg_runs_smooth,
    wicket_prob = wicket_prob_smooth
  )

exp_runs_vec <- setNames(ball_expect$avg_runs, ball_expect$ball_in_innings)
wkt_prob_vec <- setNames(ball_expect$wicket_prob, ball_expect$ball_in_innings)

avg_runs_innings <- all_balls %>%
  group_by(match_id, innings) %>%
  summarise(total = sum(runs_total), .groups = "drop") %>%
  summarise(mean(total)) %>%
  pull()

avg_wickets_innings <- all_balls %>%
  group_by(match_id, innings) %>%
  summarise(wkts = sum(wicket), .groups = "drop") %>%
  summarise(mean(wkts)) %>%
  pull()

# --- Integrated W_base (runs per expected wicket) ---

total_exp_runs <- sum(ball_expect$avg_runs, na.rm = TRUE)
total_exp_wkts <- sum(ball_expect$wicket_prob, na.rm = TRUE)

# Divide by 11 to convert team wickets → individual share scale
W_base <- total_exp_runs / (11 * total_exp_wkts)

all_balls <- all_balls %>%
  mutate(
    exp_runs = exp_runs_vec[as.character(ball_in_innings)],
    wkt_prob = wkt_prob_vec[as.character(ball_in_innings)],
    
    # Phase-adjusted wicket value
    W = W_base * (1 - wkt_prob),
    
    # Illegal deliveries don't consume expectation
    exp_runs_eff = ifelse(illegal, 0, exp_runs),
    
    # Impact
    batter_ROE = runs_batter - exp_runs_eff - batter_wicket * W,
    bowler_ROE = exp_runs_eff - runs_total + bowler_wicket * W
  )

sum(all_balls$batter_ROE) + sum(all_balls$bowler_ROE)
# should be approx: -sum(runs_total - runs_batter)

bat <- all_balls %>%
  group_by(match_id, team = batting_team, player = batter) %>%
  summarise(
    batting_impact = sum(batter_ROE, na.rm = TRUE),
    .groups = "drop"
  )

bowl <- all_balls %>%
  group_by(match_id, team = bowling_team, player = bowler) %>%
  summarise(
    bowling_impact = sum(bowler_ROE, na.rm = TRUE),
    .groups = "drop"
  )

player_match <- full_join(bat, bowl,
                          by = c("match_id", "team", "player")) %>%
  mutate(
    batting_impact = coalesce(batting_impact, 0),
    bowling_impact = coalesce(bowling_impact, 0)
  )

player_match <- player_match %>%
  group_by(match_id, team) %>%
  mutate(
    bat_sd  = sd(batting_impact),
    bowl_sd = sd(bowling_impact),
    
    bat_z = ifelse(bat_sd > 0,
                   (batting_impact - mean(batting_impact)) / bat_sd,
                   0),
    
    bowl_z = ifelse(bowl_sd > 0,
                    (bowling_impact - mean(bowling_impact)) / bowl_sd,
                    0)
  ) %>%
  ungroup()

player_match_base <- player_match
