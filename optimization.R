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

lm(total_share ~ Sold_Cr, data = auction_winners) %>%
  summary()



library(lpSolve)

price <- player_value$Sold_Cr
value <- player_value$total_share
overseas_flag <- ifelse(player_value$Overseas == "Yes", 1, 0)

n <- length(price)

# Objective
obj <- value

# Constraint matrix
mat <- rbind(
  price,                 # Budget
  rep(1, n),             # Squad size
  overseas_flag           # Overseas limit
)

dir <- c("<=", "=", "<=")
rhs <- c(15, 18, 6)

solution <- lp(
  direction = "max",
  objective.in = obj,
  const.mat = mat,
  const.dir = dir,
  const.rhs = rhs,
  all.bin = TRUE
)

selected_players <- player_value[solution$solution == 1, ]

sum(selected_players$Sold_Cr)
sum(selected_players$total_share)
sum(selected_players$Overseas == "Yes")

# Add logo path

selected_players <- selected_players %>%
  mutate(
    logo_path = paste0("logos/", Team, ".png")
  )

# 1. Prepare and Split the data
first_half <- selected_players[1:9, ]
second_half <- selected_players[10:18, ]

# Rename second half to avoid duplicate column names
colnames(second_half) <- paste0(colnames(second_half), "_2")

# Combine side-by-side
combined_wide <- cbind(first_half, second_half)

# 2. Build the 9x2 GT Table
plot_topXI_wide <- combined_wide %>%
  select(
    # Left Block
    logo_path, player, Role, Sold_Cr, total_share, WS_per_cr,
    # Right Block
    logo_path_2, player_2, Role_2, Sold_Cr_2, total_share_2, WS_per_cr_2
  ) %>%
  gt() %>%
  
  # Render logos for both sides
  text_transform(
    locations = cells_body(columns = c(logo_path, logo_path_2)),
    fn = function(x) local_image(filename = x, height = 35)
  ) %>%
  
  # Role Colors (Applied to both sides)
  data_color(
    columns = c(Role, Role_2),
    colors = scales::col_factor(
      palette = c("BAT" = "#1f77b4", "BOWL" = "#d62728", "AR" = "#2ca02c"),
      domain = NULL
    )
  ) %>%
  
  # WS per ₹ Gradient (Applied to both sides)
  data_color(
    columns = c(WS_per_cr, WS_per_cr_2),
    colors = scales::col_numeric(
      palette = c("yellow", "green", "#1a9850"),
      domain = ws_range
    )
  ) %>%
  
  # Text Styling for Role Labels
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_body(columns = c(Role, Role_2))
  ) %>%
  
  # Numeric & Currency Formatting
  fmt_currency(columns = c(Sold_Cr, Sold_Cr_2), currency = "INR", decimals = 2) %>%
  fmt_number(columns = c(total_share, total_share_2, WS_per_cr, WS_per_cr_2), decimals = 2) %>%
  
  # Clean Labels for the header
  cols_label(
    logo_path = "", player = "Player", Role = "Role", Sold_Cr = "Sold Price", 
    total_share = "Total WS", WS_per_cr = "WS/₹",
    logo_path_2 = "", player_2 = "Player", Role_2 = "Role", 
    Sold_Cr_2 = "Sold Price", total_share_2 = "Total WS", WS_per_cr_2 = "WS/₹"
  ) %>%
  
  # Visual Polish: Alignment and the "Middle Spine" Divider
  cols_align("center", columns = contains("Role") | contains("WS_per_cr")) %>%
  tab_style(
    style = cell_borders(sides = "left", color = "grey70", weight = px(3)),
    locations = cells_body(columns = logo_path_2)
  ) %>%
  
  # Optional: Table Header
  tab_header(
    title = md("**Optimal Top XI + Reserves**"),
    subtitle = "With Constraint of 15 Cr to spend, 6 Overseas Players"
  )

gtsave(plot_topXI_wide, "optimal_team_9x2_final.png")