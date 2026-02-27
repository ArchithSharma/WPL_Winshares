player_match <- player_match %>%
  group_by(match_id, team) %>%
  mutate(
    team_bat_abs  = sum(abs(bat_z), na.rm = TRUE),
    team_bowl_abs = sum(abs(bowl_z), na.rm = TRUE),
    team_total_abs = team_bat_abs + team_bowl_abs,
    
    bat_share  = ifelse(team_total_abs > 0, bat_z  / team_total_abs, 0),
    bowl_share = ifelse(team_total_abs > 0, bowl_z / team_total_abs, 0),
    
    total_share = bat_share + bowl_share
  ) %>%
  ungroup()

player_match <- player_match %>%
  group_by(match_id, team) %>%
  mutate(
    team_bat_abs = sum(abs(bat_z)),
    bat_share = ifelse(team_bat_abs > 0,
                       bat_z / team_bat_abs,
                       0)
  ) %>%
  ungroup()

player_match <- player_match %>%
  group_by(match_id, team) %>%
  mutate(
    team_bowl_abs = sum(abs(bowl_z)),
    bowl_share = ifelse(team_bowl_abs > 0,
                        bowl_z / team_bowl_abs,
                        0)
  ) %>%
  ungroup()

player_match <- player_match %>%
  mutate(
    total_share = bat_share + bowl_share
  )

player_match <- player_match %>%
  group_by(match_id, team) %>%
  mutate(
    team_bat_abs  = sum(abs(bat_z), na.rm = TRUE),
    team_bowl_abs = sum(abs(bowl_z), na.rm = TRUE),
    
    bat_share = ifelse(team_bat_abs > 0,
                       0.5 * bat_z / team_bat_abs,
                       0),
    
    bowl_share = ifelse(team_bowl_abs > 0,
                        0.5 * bowl_z / team_bowl_abs,
                        0),
    
    total_share = 2 * (bat_share + bowl_share)
  ) %>%
  ungroup()


season_shares <- player_match %>%
  group_by(player) %>%
  summarise(
    matches = n_distinct(match_id),
    bat_share_total  = sum(bat_share),
    bowl_share_total = sum(bowl_share),
    total_share      = sum(total_share),
    avg_match_share  = mean(total_share),
    .groups = "drop"
  ) %>%
  arrange(desc(total_share))

season_shares <- season_shares %>%
  mutate(
    name_key = clean_name_key(player),
    
    # last name logic (handles de Klerk etc.)
    last = case_when(
      str_detect(name_key, " de ") ~ str_extract(name_key, "de .*"),
      TRUE ~ word(name_key, -1)
    )
  )

clean_name_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z\\- ]", "") %>%   # keep letters, space, hyphen
    str_squish()
}

auction <- read.csv("auction.csv")
auction <- auction %>%
  mutate(
    name_key = clean_name_key(Player),
    
    last = case_when(
      str_detect(name_key, " de ") ~ str_extract(name_key, "de .*"),
      TRUE ~ word(name_key, -1)
    )
  )

# Get last names in player match that aren't in auction
missing_last <- setdiff(season_shares$last, auction$last)
# Correct Arundhati Reddy to RCB
auction <- auction %>%
  mutate(
    Team = ifelse(Player == "Arundhati Reddy", "RCB", Team))

player_value <- season_shares %>%
  left_join(
    auction %>% select(last, Team, Sold_Cr, Role, Overseas),
    by = "last"
  )
player_value <- player_value %>%
  distinct(player, .keep_all = TRUE)
# Fix A Reddy 
player_value <- player_value %>%
  mutate(
    Team = ifelse(player == "A Reddy", "RCB", Team),
    Sold_Cr = ifelse(player == "A Reddy", 0.75, Sold_Cr),
    Role = ifelse(player == "A Reddy", "AR", Role)
  )
# Fix DB Sharma
player_value <- player_value %>%
  mutate(
    Team = ifelse(player == "DB Sharma", "UPW", Team),
    Sold_Cr = ifelse(player == "DB Sharma", 3.2, Sold_Cr),
    Role = ifelse(player == "DB Sharma", "AR", Role)
  )
# Fix AB Sharma
player_value <- player_value %>%
  mutate(
    Team = ifelse(player == "AB Sharma", "GG", Team),
    Sold_Cr = ifelse(player == "AB Sharma", 0.45, Sold_Cr),
    Role = ifelse(player == "AB Sharma", "AR", Role)
  )
# Fix Vaishnavi Sharma
player_value <- player_value %>%
  mutate(
    Team = ifelse(player == "Vaishnavi Sharma", "MI", Team),
    Sold_Cr = ifelse(player == "Vaishnavi Sharma", 0.3, Sold_Cr),
    Role = ifelse(player == "Vaishnavi Sharma", "BOWL", Role)
  )
# Fix RP Yadav
player_value <- player_value %>%
  mutate(
    Team = ifelse(player == "RP Yadav", "RCB", Team),
    Sold_Cr = ifelse(player == "RP Yadav", 0.65, Sold_Cr),
    Role = ifelse(player == "RP Yadav", "AR", Role)
  )
# Fix H Kaur
player_value <- player_value %>%
  mutate(
    Team = ifelse(player == "H Kaur", "MI", Team),
    Sold_Cr = ifelse(player == "H Kaur", 2.5, Sold_Cr),
    Role = ifelse(player == "H Kaur", "BAT", Role)
  )

# players in player_value, but not in auction
missing_in_auction <- setdiff(player_value$last, auction$last)

player_value <- player_value %>%
  mutate(
    cr_per_WS = case_when(
      is.na(Sold_Cr) ~ NA_real_,
      total_share <= 0 ~ NA_real_,
      TRUE ~ Sold_Cr / total_share
    ),
    
    WS_per_cr = case_when(
      is.na(Sold_Cr) ~ NA_real_,
      Sold_Cr == 0 ~ NA_real_,
      TRUE ~ total_share / Sold_Cr
    )
  )

auction_winners <- player_value %>%
  filter(!is.na(WS_per_cr)) %>%
  arrange(desc(WS_per_cr)) %>%
  select(player, Team, Sold_Cr, total_share, WS_per_cr, Role, Overseas)
