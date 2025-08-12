library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(stringr)

defense_summary <- defense_summary %>%
  #left_join(pass_rush_summary, by = "player") %>%
  left_join(NFL_logos_colors, by = "team_name")
  #select(!ends_with(".y"))
  #rename_with(~ sub("\\.x$", "", .x), ends_with(".x"))

edge_sum <- defense_summary %>%
  filter(position == "ED") %>%
  filter(!is.na(sacks)) %>%
  mutate(havoc = sacks+batted_passes+tackles_for_loss+forced_fumbles+interceptions) %>%
  arrange(desc(sacks)) %>%
  slice_head(n = 10) %>%   #this needs to run before everything else does
  select(
    player,
    team_logo_wiki,
    sacks,
    pass_rush_win_rate,
    missed_tackle_rate,
    grades_run_defense,
    havoc
  ) %>%
  gt() %>%
  tab_header(
    title = "Top Edge Rushers in 2024",
    subtitle = "Top 10 by sack total | Stats: PFF | By: Connor Dague"
  ) %>%
  tab_options(
    column_labels.font.weight = "bold"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = player)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = px(18)),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(size = px(18)),
    locations = cells_body()
  ) %>%
  text_transform(
    locations = cells_body(columns = team_logo_wiki),
    fn = function(x) {
      web_image(url = x, height = 30)  # adjust height if needed
    }
  ) %>%
  cols_align(
    align = "center",
    columns = c(team_logo_wiki,sacks,pass_rush_win_rate,missed_tackle_rate,grades_run_defense,havoc)
  ) %>%
  cols_label(
    player = "Player",
    team_logo_wiki = "",
    sacks = "Sacks",
    pass_rush_win_rate = "Pass Rush Win %",
    missed_tackle_rate = "Missed Tackle %",
    grades_run_defense = "PFF Run Grade",
    havoc = "Havoc Plays"
  ) %>%
  cols_width(
    team_logo_wiki ~ px(30),
    player ~ px(175)
  ) %>%
  data_color(
    columns = c(pass_rush_win_rate, grades_run_defense, havoc),
    colors = scales::col_numeric(
      palette = c("blue", "white", "green"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = missed_tackle_rate,
    colors = scales::col_numeric(
      palette = c("green", "white", "blue"),  # lower is better for missed tackle rate
      domain = NULL
    )
  )


edge_sum

