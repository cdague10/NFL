library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

##################
middlerun_success <- pbp_rp %>%
  filter(rush == 1, run_gap== "guard",!is.na(rusher_player_name))|>
  group_by(posteam) |>
  summarize(rushes = n(),
            epa_rush = mean(epa),
            ) |>
  filter(rushes >= 10) |>
  arrange(-epa_rush)

outsiderun_success <- pbp_rp %>%
  filter(rush == 1, run_gap== "end",!is.na(rusher_player_name))|>
  group_by(posteam) |>
  summarize(rushes = n(),
            epa_rush = mean(epa),
  ) |>
  filter(rushes >= 10) |>
  arrange(-epa_rush)

totalrun_eff <- middlerun_success |>
  left_join(outsiderun_success, by = "posteam")

# Join the team logos with total_eff
totalrun_eff <- totalrun_eff |>
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

totalrun_eff |>
  ggplot(aes(x = epa_rush.x, y = epa_rush.y)) +
  geom_hline(yintercept = mean(totalrun_eff$epa_rush.y), linetype = "dashed") +
  geom_vline(xintercept = mean(totalrun_eff$epa_rush.x), linetype = "dashed") +
  geom_smooth(method = "glm") +
  geom_image(aes(image = team_logo_espn), size = 0.06, asp = 16/9) +
  theme_minimal() +
  labs(x = "EPA/Run between the guards",
       y = "EPA/Run off tackle",
       title = "EPA/Run by run locations in 2024",
       subtitle = "Regular season through week 17",
       caption = "By: Connor Dague | @cdague10") +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

########################

middlerun_def_success <- pbp_rp %>%
  filter(rush == 1, run_gap!= "end",!is.na(rusher_player_name))|>
  group_by(defteam) |>
  summarize(rushes = n(),
            epa_rush = mean(epa),
  ) |>
  filter(rushes >= 10) |>
  arrange(-epa_rush)

outsiderun_def_success <- pbp_rp %>%
  filter(rush == 1, run_gap== "end",!is.na(rusher_player_name))|>
  group_by(defteam) |>
  summarize(rushes = n(),
            epa_rush = mean(epa),
  ) |>
  filter(rushes >= 10) |>
  arrange(-epa_rush)

totalrun_def_eff <- middlerun_def_success |>
  left_join(outsiderun_def_success, by = "defteam")

# Join the team logos with total_eff
totalrun_def_eff <- totalrun_eff |>
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))

totalrun_def_eff |>
  ggplot(aes(x = epa_rush.x, y = epa_rush.y)) +
  geom_hline(yintercept = mean(totalrun_eff$epa_rush.y), linetype = "dashed") +
  geom_vline(xintercept = mean(totalrun_eff$epa_rush.x), linetype = "dashed") +
  geom_smooth(method = "glm") +
  geom_image(aes(image = team_logo_espn), size = 0.06, asp = 16/9) +
  theme_minimal() +
  labs(x = "EPA/Run allowed between the tackles",
       y = "EPA/Run allowed off the edge",
       title = " Defensive EPA/Run by run locations in 2024",
       subtitle = "Regular season through week 17",
       caption = "By: Connor Dague | @cdague10") +
  theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

###########
names(pbp)
pbp24 <- load_pbp(2024)
pbp24_rp <- pbp24 |>
  filter(pass == 1 | rush == 1) |>
  # and get plays that have epa
  filter(!is.na(epa))

rushing_eff <- pbp24_rp %>%
  #filter(posteam = ()) %>%
  filter(!is.na(rusher_player_name)) %>%
  group_by(rusher_player_name) %>%
  summarize(rushes = n(),
            avg_epa = mean(epa),
            team_abbr= last(posteam)) %>%
  filter(rushes >= 136) %>%
  left_join(teams_colors_logos, by = "team_abbr")
arrange(-avg_epa)

rushing_eff %>%
  ggplot(aes(x= avg_epa, y= fct_reorder(rusher_player_name,avg_epa)))+
  geom_bar(aes(fill= team_color, color=team_color2),stat = "identity")+
  scale_color_identity(aesthetics = c("fill","color"))+
  theme_bw() +
  labs(x= "EPA/Run",
       y= "Rusher Name",
       title= "Who are the most efficient RBs this year?",
       subtitle="2024, minimum of 135 rushes")+
  theme(legend.position="none",
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12,hjust=0.5),
        panel.grid.major.y = element_line(size=0.1))
