library(tidyverse)
library(ggimage)
library(gt)
library(gtExtras)
library(ggplot2)
library(dplyr)
library(tidymodels)
library(nflreadr)
library(lpSolve)

#ffrankings <- load_ff_opportunity(2024)

Fantasy_CheatSheet <- FantasyPros_2025_Overall_ADP_Rankings %>%
  mutate(TEAM = recode(
    TEAM,
    "LAR" = "LA",
    "JAC" = "JAX",
    "HOU" = "HST",
    "ARI" = "ARZ",
    "BAL" = "BLT",
    "CLE" = "CLV"
  )) %>%
  mutate(POS = str_remove_all(POS, "[0-9]")) %>%
  filter(AVG <= 200) %>%
  arrange(AVG) %>%
  #slice(-(1:6)) %>%
  filter(!`PLAYER NAME` %in% c("Conner Heyward", "Kyle Pitts")) %>%
  left_join(FantasyPros_2025_Draft_ALL_Rankings, by = "PLAYER NAME")%>%
  left_join(NFL_logos_colors, by = c("TEAM.x" = "team_name"))






#Fantasy Draft Picks
my_picks <- c(1, 20, 21, 40, 41, 60, 61)
drafted_players <- character()
final_team <- data.frame()

# Total position constraints
needed_qb <- 1
needed_rb <- 2
needed_wr <- 2
needed_te <- 1
needed_flex <- 1

for (pick in my_picks) {
  if (pick <= 10) {  # First Round
    adp_lower <- pick - 2
    adp_upper <- pick + 4
  } else if (pick <= 30) {  # Rounds 2-3
    adp_lower <- pick - 4
    adp_upper <- pick + 5
  } else if (pick <= 50) {  # Rounds 4-5
    adp_lower <- pick - 5
    adp_upper <- pick + 10
  } else {  # Late rounds (Rounds 6+)
    adp_lower <- pick - 6
    adp_upper <- pick + 12
  }
  # Filter available players
  available_pool <- Fantasy_CheatSheet %>%
    filter(!is.na(AVG)) %>%
    filter(AVG >= adp_lower & AVG <= adp_upper) %>%
    filter(!`PLAYER NAME` %in% drafted_players)

  if (nrow(available_pool) == 0) next

  # Only keep players who fulfill a needed position
  available_pool <- available_pool %>%
    filter(
      (POS == "QB" & needed_qb > 0) |
        (POS == "RB" & (needed_rb > 0 | needed_flex > 0)) |
        (POS == "WR" & (needed_wr > 0 | needed_flex > 0)) |
        (POS == "TE" & (needed_te > 0 | needed_flex > 0))
    )
  if (pick == 1) {
    available_pool <- available_pool %>% filter(POS != "QB")
  }
  if (nrow(available_pool) == 0) next

  # Maximize FANTASYPTS for one pick
  objective <- available_pool$PROJPTS
  constraint_matrix <- matrix(1, nrow = 1, ncol = nrow(available_pool))
  rhs <- 1
  dir <- "=="

  sol <- lp(
    direction = "max",
    objective.in = objective,
    const.mat = constraint_matrix,
    const.dir = dir,
    const.rhs = rhs,
    all.bin = TRUE
  )

  if (sol$status == 0 && sum(sol$solution == 1) == 1) {
    selected_index <- which(sol$solution == 1)

    if (length(selected_index) == 1) {
      selected <- available_pool[selected_index, ]
      selected$Draft_Pick <- pick
      final_team <- bind_rows(final_team, selected)
      drafted_players <- c(drafted_players, selected$`PLAYER NAME`)

      # Update needs
      if (selected$POS == "QB") needed_qb <- needed_qb - 1
      else if (selected$POS == "RB" && needed_rb > 0) needed_rb <- needed_rb - 1
      else if (selected$POS == "WR" && needed_wr > 0) needed_wr <- needed_wr - 1
      else if (selected$POS == "TE" && needed_te > 0) needed_te <- needed_te - 1
      else needed_flex <- needed_flex - 1
    }
  }
  if (sum(sol$solution == 1) != 1) {
    print(paste("Invalid solution at pick", pick))
    print(sol$solution)
    next
  }
  if (nrow(final_team) == 7) break
}

# Show result
final_team <- final_team %>% arrange(Draft_Pick)
print(final_team)




final_table <- final_team %>%
  mutate(
    Round = ceiling(Draft_Pick / 10),
    Pick_in_Round = ifelse(Draft_Pick %% 10 == 0, 10, Draft_Pick %% 10)
  ) %>%
  select(
    Position = POS,
    Player = `PLAYER NAME`,
    Team = team_wordmark,
    Round,
    Pick = Draft_Pick,
    ADP = AVG,
    `Fantasy Points` = PROJPTS
  )


gt_table <- final_table %>%
  gt() %>%
  text_transform(
    locations = cells_body(vars(Team)),
    fn = function(x) {
      web_image(url = x, height = 25)
    }
  ) %>%
  cols_label(
    Team = "Team",
    Round = "Round",
    Pick = "Pick",
    ADP = "ADP",
    `Fantasy Points` = "Projected Pts"
  ) %>%
  tab_header(
    title = "Optimal Fantasy Team - First Pick",
    subtitle = "10-man Snake | ADP: Average of ESPN, CBS, Sleeper, NFL drafts | Proj Pts: NFL Fantasy"
  ) %>%
  fmt_number(
    columns = vars(`Fantasy Points`),
    decimals = 1
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  gt_theme_nytimes() %>%
  tab_style(
    style = list(
      cell_fill(color = "#E3e3e3"),  # uniform light grey
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = vars(`Fantasy Points`))
  )

gt_table
