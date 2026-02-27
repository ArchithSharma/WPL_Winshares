library(gt)
library(dplyr)
create_10x2_player_table <- function(df, title_text, color_col) {
  # 1. Capture the global min/max for the gradient BEFORE splitting
  # This ensures the gradient flows from Rank 1 to Rank 20 correctly
  global_min <- min(df[[color_col]], na.rm = TRUE)
  global_max <- max(df[[color_col]], na.rm = TRUE)
  
  # 2. Split into 10x2
  first_half <- df[1:10, ]
  second_half <- df[11:20, ]
  colnames(second_half) <- paste0(colnames(second_half), "_2")
  combined <- cbind(first_half, second_half)
  
  # 3. Build Table
  combined %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title_text, "**"))
    ) %>%
    
    # Images
    text_transform(
      locations = cells_body(columns = c(logo_path, logo_path_2)),
      fn = function(x) local_image(filename = x, height = 35)
    ) %>%
    
    # Role Colors (Static factors, so no domain issue here)
    data_color(
      columns = c(Role, Role_2),
      colors = scales::col_factor(
        palette = c("BAT" = "#1f77b4", "BOWL" = "#d62728", "AR" = "#2ca02c"),
        domain = NULL
      )
    ) %>%
    
    # METRIC GRADIENT - Fixed with 'domain'
    data_color(
      columns = c(!!sym(color_col), !!sym(paste0(color_col, "_2"))),
      colors = scales::col_numeric(
        palette = if(title_text == "Worst Performing Players") 
          c("#800000", "red", "#FF2C2C") else 
            c("yellow", "green", "#1a9850"),
        domain = c(global_min, global_max) # <--- THIS FIXES THE GRADIENT
      )
    ) %>%
    
    # Formatting & Styles
    tab_style(style = cell_text(color = "white", weight = "bold"), 
              locations = cells_body(columns = c(Role, Role_2))) %>%
    fmt_currency(columns = c(Sold_Cr, Sold_Cr_2), currency = "INR", decimals = 2) %>%
    fmt_number(columns = contains("total_share") | contains("WS_per_cr"), decimals = 2) %>%
    
    # Labels & Divider
    cols_label(logo_path="", player="Player", Role="Role", Sold_Cr="Price", total_share="WS", WS_per_cr="WS/₹",
               logo_path_2="", player_2="Player", Role_2="Role", Sold_Cr_2="Price", total_share_2="WS", WS_per_cr_2="WS/₹") %>%
    
    tab_style(style = cell_borders(sides = "left", color = "grey70", weight = px(3)), 
              locations = cells_body(columns = logo_path_2)) %>%
    cols_align("center", columns = contains("Role") | contains("WS_per_cr"))
}

# Add logo path
auction_winners <- auction_winners %>%
  mutate(logo_path = paste0("logos/", Team, ".png"))
# Best Value
best_value_data <- auction_winners %>% arrange(desc(WS_per_cr)) %>% head(20) %>%
  select(logo_path, player, Role, Sold_Cr, total_share, WS_per_cr)
gtsave(create_10x2_player_table(best_value_data, "Best Value Players", "WS_per_cr"), "best_value_players.png")

# Most Expensive
most_exp_data <- auction_winners %>% arrange(desc(Sold_Cr)) %>% head(20) %>%
  select(logo_path, player, Role, Sold_Cr, total_share, WS_per_cr)
gtsave(create_10x2_player_table(most_exp_data, "Most Expensive Players", "Sold_Cr"), "most_expensive_players.png")

# Most Win Shares
by_shares_data <- auction_winners %>% arrange(desc(total_share)) %>% head(20) %>%
  select(logo_path, player, Role, Sold_Cr, total_share, WS_per_cr)
gtsave(create_10x2_player_table(by_shares_data, "Top Performers by Win Shares", "total_share"), "most_win_shares.png")

# Worst Players
worst_data <- auction_winners %>% arrange(total_share) %>% head(20) %>%
  select(logo_path, player, Role, Sold_Cr, total_share, WS_per_cr)
gtsave(create_10x2_player_table(worst_data, "Worst Performing Players", "total_share"), "worst_players.png")