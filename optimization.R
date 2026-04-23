# Plot auction price vs win shares, color points by team
library(ggplot2)
ggplot(auction_winners, aes(x = Sold_Cr, y = total_share, color = Team)) +
  # specify colors based on RCB, DC, UPW, MI, GG colors
  scale_color_manual(values = c(
    "RCB" = "#A4243B",
    "DC" = "#007AB8",
    "UPW" = "#FF6F61",
    "MI" = "#004BA0",
    "GG" = "#FBB117"
  )) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Auction Price vs Total Win Shares",
    x = "Sold Price (Rs Cr)",
    y = "Total Win Shares"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

lm(total_share ~ Sold_Cr + Role, data = auction_winners) %>%
  summary()



library(lpSolve)
library(gt)
library(tidyverse)

# 1. PRE-FILTER: Ensure the pool only contains players with >= 2 matches
# This fixes the filtering error where the solver was summing matches instead of filtering individuals
eligible_pool <- player_value %>% 
  filter(matches >= 2)

# 2. Extract Vectors for LP Solver
price         <- eligible_pool$Sold_Cr
value         <- eligible_pool$total_share
overseas_flag <- ifelse(eligible_pool$Overseas == "Yes", 1, 0)
n             <- length(price)

# 3. Setup Linear Programming
# Objective: Maximize total_share
obj <- value

# Constraint Matrix (Budget, Squad Size, Overseas Limit)
mat <- rbind(
  price,          # Total Budget <= 15
  rep(1, n),      # Squad Size = 18
  overseas_flag   # Overseas Players <= 6
)

dir <- c("<=", "=", "<=")
rhs <- c(15, 18, 6)

# Solve
solution <- lp(
  direction = "max",
  objective.in = obj,
  const.mat = mat,
  const.dir = dir,
  const.rhs = rhs,
  all.bin = TRUE
)

# 4. Extract Selected Players
selected_players <- eligible_pool[solution$solution == 1, ]

# 5. Prepare Table Data (Splitting into 9x2 layout)
selected_players <- selected_players %>%
  mutate(logo_path = paste0("logos/", Team, ".png"))

# Split the 18 players into two groups of 9
first_half  <- selected_players[1:9, ]
second_half <- selected_players[10:18, ]
ws_range    <- range(selected_players$WS_per_cr, na.rm = TRUE)

# Rename second half to avoid duplicate column names for cbind
colnames(second_half) <- paste0(colnames(second_half), "_2")

# Combine side-by-side
combined_wide <- cbind(first_half, second_half)

# 6. Build the GT Table
plot_topXI_wide <- combined_wide %>%
  select(
    logo_path, player, Role, Sold_Cr, total_share, WS_per_cr,
    logo_path_2, player_2, Role_2, Sold_Cr_2, total_share_2, WS_per_cr_2
  ) %>%
  gt() %>%
  
  # Render logos
  text_transform(
    locations = cells_body(columns = c(logo_path, logo_path_2)),
    fn = function(x) local_image(filename = x, height = 35)
  ) %>%
  
  # Role Colors
  data_color(
    columns = c(Role, Role_2),
    colors = scales::col_factor(
      palette = c("BAT" = "#1f77b4", "BOWL" = "#d62728", "AR" = "#2ca02c"),
      domain = NULL
    )
  ) %>%
  
  # WS per Cr Gradient
  data_color(
    columns = c(WS_per_cr, WS_per_cr_2),
    colors = scales::col_numeric(
      palette = c("yellow", "green", "#1a9850"),
      domain = ws_range
    )
  ) %>%
  
  # Styling
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_body(columns = c(Role, Role_2))
  ) %>%
  fmt_currency(columns = c(Sold_Cr, Sold_Cr_2), currency = "INR", decimals = 2) %>%
  fmt_number(columns = c(total_share, total_share_2, WS_per_cr, WS_per_cr_2), decimals = 2) %>%
  
  # Column Labels
  cols_label(
    logo_path = "", player = "Player", Role = "Role", Sold_Cr = "Price", 
    total_share = "Total WS", WS_per_cr = "WS/₹",
    logo_path_2 = "", player_2 = "Player", Role_2 = "Role", 
    Sold_Cr_2 = "Price", total_share_2 = "Total WS", WS_per_cr_2 = "WS/₹"
  ) %>%
  
  cols_align("center", columns = contains("Role") | contains("WS_per_cr")) %>%
  tab_style(
    style = cell_borders(sides = "left", color = "grey70", weight = px(3)),
    locations = cells_body(columns = logo_path_2)
  ) %>%
  
  tab_header(
    title = md("**Optimal Top XI + Reserves**"),
    subtitle = "Budget: 15 Cr | Squad: 18 | Overseas: Max 6 | Min 2 Matches Played"
  )

# Save output
gtsave(plot_topXI_wide, "optimal_team_9x2_final.png")

# 7. Regression Analysis on selected team
lm(total_share ~ Sold_Cr, data = player_value) %>%
  summary()

