library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)

pbp <- load_pbp(2024)

#Yardage Distribution over last 5 years
pbp_last5 <- load_pbp(2019:2024)
kick_stats_by_yardline <- pbp_last5 %>%
  filter(play_type == "punt") %>%
  group_by(yardline_100) %>%
  summarize(
    avg_kick_distance = mean(kick_distance, na.rm = TRUE),
    punts = n(),
    max_kick_distance = max(kick_distance, na.rm = TRUE),
    min_kick_distance = min(kick_distance, na.rm = TRUE)
  ) %>%
  arrange(yardline_100)

pbp_punt <- pbp %>%
  filter(play_type == "punt") %>%
  filter(!punter_player_name %in% c("C.Boswell","C.Johnston","C.Ryland","T.Zentner","J.Browning","J.Camarda","M.Palardy","M.Haack","M.Wishnowsky"))%>%
  mutate(
    end_yardline = yardline_100-kick_distance,
    end_yardline = if_else(end_yardline == 0, 20,end_yardline)
  ) %>%
  group_by(yardline_100) %>%
  filter(n() >= 2) %>%
  mutate(
    punt_percentile = percent_rank(yardline_100-end_yardline)*100
  ) %>%
  ungroup()

punt_sum<-pbp_punt%>%
  group_by(punter_player_name) %>%
  summarize(punts = n(),
            punter_eff = mean(punt_percentile),
            team_abbr= last(posteam))%>%
  filter(punts>=35)%>%
  left_join(teams_colors_logos, by = "team_abbr")

punt_sum %>%
  ggplot(aes(x= punter_eff, y= fct_reorder(punter_player_name, punter_eff)))+
  geom_bar(aes(fill= team_color, color=team_color2),stat = "identity")+
  scale_color_identity(aesthetics = c("fill","color"))+
  theme_bw() +
  labs(x= "Avg Starting Position",
       y= "Punter Name",
       title= "Who's the best Punter at pinning teams deep?",
       caption= "By Connor Dague | @cdague10",
       subtitle="2024, minimum of 35 punts")+
  theme(legend.position="none",
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12,hjust=0.5),
        panel.grid.major.y = element_line(size=0.1))


pbp_punt_plot <- pbp_punt %>%
  filter(punter_player_name %in% (pbp_punt %>%
                                    count(punter_player_name) %>%
                                    filter(n >= 35) %>%
                                    pull(punter_player_name))) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# Boxplot
ggplot(pbp_punt_plot, aes(x = fct_reorder(punter_player_name, punt_percentile, .fun = median, .desc=TRUE),
                          y = punt_percentile)) +
  geom_boxplot(aes(fill = team_color, color = team_color2), width = 0.6, outlier.shape = NA) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_bw() +
  labs(x = "Punter Name",
       y = "Punt Percentile",
       title = "Which punters are the most efficient?",
       subtitle = "2024 Season | Min 35 punts",
       caption = "Net yds/punt in relation to all possible outcomes | @cdague10") +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(size = 0.2))
