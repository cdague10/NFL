# install.packages("tidyverse")
# install.packages("nflfastR")
# install.packages("ggimage")
# install.packages("gt")
# install.packages("gtExtras")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

pbp <- load_pbp(2017:2024)
nrow(pbp)
pbp |> head()

names(pbp)

pbp_rp <- pbp |>
  filter(pass == 1 | rush == 1) |>
  filter(!is.na(epa))

# Who was the Lions most efficient runner?
pbp_rp |>
  filter(posteam == "DET", rush == 1, !is.na(rusher_player_name)) |>
  group_by(rusher_player_name) |>
  summarize(rushes = n(),
            epa_rush = mean(epa)) |>
  filter(rushes >= 10) |>
  arrange(-epa_rush)

# Who was the Steelers best quarterback last season?
pbp_rp |>
  filter(posteam == "PIT", !is.na(id)) |>
  group_by(id) |>
  summarize(name = first(name),
            plays = n(),
            epa_per_play = mean(epa),
            pass_attempts = sum(complete_pass + incomplete_pass, na.rm = T)) |>
  filter(plays >= 70, pass_attempts >= 20) |>
  arrange(-epa_per_play)

################################################################
# Compare pass efficiency vs. rush efficiency last season
pass_efficiency_24 <- pbp |>
  filter(season == 2024, pass == 1) |>
  group_by(posteam) |>
  summarize(passes = n(),
            pass_epa = mean(epa))

rush_efficiency_24 <- pbp |>
  filter(season == 2024, rush == 1) |>
  group_by(posteam) |>
  summarize(rushes = n(),
            rush_epa = mean(epa))

# Join the two datasets together
total_eff <- pass_efficiency_24 |>
  left_join(rush_efficiency_24, by = "posteam")

# Join the team logos with total_eff
total_eff <- total_eff |>
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# Make the plot
total_eff |>
  ggplot(aes(x = pass_epa, y = rush_epa)) +
  geom_hline(yintercept = mean(total_eff$rush_epa), linetype = "dashed") +
  geom_vline(xintercept = mean(total_eff$pass_epa), linetype = "dashed") +
  geom_smooth(method = "glm") +
  geom_image(aes(image = team_logo_espn), size = 0.06, asp = 16/9) +
  theme_minimal() +
  labs(x = "EPA/pass",
       y = "EPA/rush",
       title = "EPA/pass and EPA/Rush in 2024",
       subtitle = "Regular season through week 17",
       caption = "By: Connor Dague | @cdague10") +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #put the title and subtitle in the middle

#############################################################################
pbp <- load_pbp(2023:2024)
agg_23 <- pbp |>
  mutate(yards_past_sticks = air_yards - ydstogo) |>
  filter(season == 2023, down %in% c(3, 4), !is.na(passer_player_id)) |>
  group_by(passer_player_id) |>
  summarize(name = first(name),
            passes_23 = n(),
            agg_23 = mean(yards_past_sticks, na.rm = T))

agg_24 <- pbp |>
  mutate(yards_past_sticks = air_yards - ydstogo) |>
  filter(season == 2024, down %in% c(3, 4), !is.na(passer_player_id)) |>
  group_by(passer_player_id) |>
  summarize(passes_24 = n(),
            team = last(posteam),
            agg_24 = mean(yards_past_sticks, na.rm = T))

agg_23_24 <- agg_23 |>
  left_join(agg_24, by = "passer_player_id") |>
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) |>
  filter(passes_23 >= 100)

agg_23_24 |>
  mutate(agg_23 = round(agg_23, 1),
         agg_24 = round(agg_24, 1),
         diff = agg_24 - agg_23) |>
  select(name, team_wordmark, agg_23, agg_24, diff) |>
  arrange(-diff) |>
  gt() |>
  cols_align(align = "center") |>
  gtExtras::gt_img_rows(team_wordmark) |>
  cols_label(name = "Quarterback",
             team_wordmark = "",
             agg_21 = "Late-Down Yards past sticks, 2023",
             agg_22 = "Late-Down Yards past sticks, 2024",
             diff = "Difference") |>
  gtExtras::gt_theme_espn()


