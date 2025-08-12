library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggrepel)

pbp_last5 <- load_pbp(2020:2024)
pbp24 <- load_pbp(2024)
pbp_00_10 <- load_pbp(2000:2010)

kick_stats_00_10 <- pbp_00_10 %>%
  filter(play_type == "field_goal",
         yardline_100 <= 66) %>%
  mutate(kick_yardline = yardline_100 + 18) %>%
  group_by(kick_yardline) %>%
  summarize(
    make = sum(field_goal_result == "made", na.rm = TRUE),
    miss = sum(field_goal_result %in% c("missed", "blocked"), na.rm = TRUE),
    attempts = make + miss,
    expected_make_percentage = make / attempts
  ) %>%
  arrange(kick_yardline)

kick_stats_by_yardlinelast5 <- pbp_last5 %>%
  filter(play_type == "field_goal",
         yardline_100 <= 66) %>%
  mutate(kick_yardline = yardline_100 + 18) %>%
  group_by(kick_yardline) %>%
  summarize(
    make = sum(field_goal_result == "made", na.rm = TRUE),
    miss = sum(field_goal_result %in% c("missed", "blocked"), na.rm = TRUE),
    attempts = make + miss,
    expected_make_percentage = make / attempts
  ) %>%
  arrange(kick_yardline)

kick_stats_00_10 <- kick_stats_00_10 %>%
  mutate(group = "2000-2010")
kick_stats_by_yardlinelast5 <- kick_stats_by_yardlinelast5 %>%
  mutate(group = "2020-2025")

combined_data <- bind_rows(kick_stats_00_10, kick_stats_by_yardlinelast5) %>%
  rename(x = kick_yardline, value = expected_make_percentage)

ggplot(combined_data, aes(x = x, y = value, fill = group)) +
  geom_area(alpha = 0.9, position = "identity") +
  scale_fill_manual(
    values = c("2000-2010" = "blue", "2020-2025" = "green")
  ) +
  scale_x_continuous(
    limits = c(20, 65),
    breaks = seq(20, 65, by = 5),
    labels = function(x) paste0(x)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy=25)) +
  labs(
    title = "NFL Field Goal Percentage by Yard Line",
    caption = "Data: nflfastR | By: Connor Dague",
    x = "Yard Line",
    y = "Make Percentage"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "right"
  )+
  annotate("label",
             x = 60, y = 0.93,
             label = "2000-2010",
             fill = "#0bda51",
             color = "black",
             size = 3,
             label.size = 0.5) +
  annotate("label",
           x = 60, y = 0.86,
           label = "2020-2025",
           fill = "green",
           color = "black",
           size = 3,
           label.size = 0.5)

#Kicker Production vs Expected
kicker_sum <- pbp24 %>%
  mutate(kick_yardline = yardline_100 + 8) %>%
  left_join(kick_stats_by_yardline %>% select(kick_yardline, expected_make_percentage),
            by = "kick_yardline") %>%
  filter(play_type == "field_goal") %>%
  mutate(
    posteam = if_else(
      kicker_player_name == "A.Carlson" & posteam %in% c("NYJ", "SF"),
      "NYJ",
      posteam
    )
  ) %>%
  group_by(kicker_player_name, posteam) %>%
  summarise(
    made = sum(field_goal_result == "made", na.rm = TRUE),
    miss = sum(field_goal_result %in% c("missed", "blocked"), na.rm = TRUE),
    expected_fg = sum(expected_make_percentage, na.rm = TRUE)
  ) %>%
  filter(made >= 17) %>%
  left_join(teams_colors_logos,by = c("posteam" = "team_abbr"))

kicker_sum %>%
  ggplot(aes(x=expected_fg, y=made)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_text_repel(
    aes(label = kicker_player_name, color = team_color),
    size = 2.9,
    max.overlaps = 50,
    box.padding = 0.1,
    point.padding = 0.2,
    segment.color = "grey"
  ) +
  scale_color_identity()+
  annotate(
    "label",
    x = 22.5, y = 37.5,  # where to place it
    label = "Overperforming",
    fill = "white",
    color = "green",
    size = 4,
    label.size = 0.5
  ) +
  annotate(
    "label",
    x = 37.5, y = 22.5,  # where to place it
    label = "Underperforming",
    fill = "white",
    color = "red",
    size = 4,
    label.size = 0.5
  ) +
  theme_minimal() +
  labs(x = "Expected Field Goals",
       y = "Made Field Goals",
       title= "       Field Goals Made over Expected in 2024 (Min 17 FG Made)",
       caption= "nflfastR | By: Connor Dague")
