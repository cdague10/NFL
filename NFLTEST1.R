#install.packages("tidyverse")
#install.packages("nflfastR")
#install.packages("ggimage")
#install.packages("gt")
#install.packages("gtExtras")
#install.packages("ggplot2")
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
pbp <- load_pbp(2023:2024)
names(pbp)

pbp |> select(posteam, defteam, down, ydstogo, play_type, yards_gained, cpoe, air_epa,punter_player_name,epa)
#Only runs and passes
pbp_rp <- pbp |>
  filter(pass == 1 | rush == 1) |>
  # and get plays that have epa
  filter(!is.na(epa))

offenses_23 <-pbp_rp %>%
  filter(season == 2023) %>%
  group_by(posteam) %>%
  summarize(epa_23 = mean(epa))

offenses_24 <-pbp_rp %>%
  filter(season == 2024) %>%
  group_by(posteam) %>%
  summarize(epa_24 = mean(epa))

offenses_all <- offenses_23 %>%
  left_join(offenses_24, by = "posteam")
view(teams_colors_logos)

offenses_all <- offenses_all %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

offenses_all %>%
  ggplot(aes(x=epa_23, y=epa_24)) +
  geom_hline(yintercept = mean(offenses_all$epa_24),linetype= "dashed") +
  geom_vline(xintercept = mean(offenses_all$epa_23),linetype= "dashed")+
  geom_image(aes(image = team_logo_espn), size=0.05, asp=16/9) +
  geom_smooth(method="glm")
  theme_minimal() +
  labs(x = "Offensive EPA/Play in 2023",
       y = "Offensive EPA/Play in 2024",
       title= "Offensive EPA/Play in 2023 compared to 2024",
       caption= "By Connor Dague | @cdague10 ")

############################

score_differentical_24 <-pbp_rp %>%
  filter(season == 2024) %>%
  group_by(posteam) %>%
  summarize(score_diff_24 = mean(score_differential))
view(score_differentical_24)

##############

third_long <- pbp_rp %>%
  #filter(posteam = ()) %>%
  filter(down == 3) %>%
  filter(ydstogo >=8) %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarize(passes = n(),
            avg_epa = mean(epa),
            team_abbr= last(posteam)) %>%
  filter(passes >= 70) %>%
  left_join(teams_colors_logos, by = "team_abbr")
  arrange(-avg_epa)

third_long %>%
  ggplot(aes(x= avg_epa, y= fct_reorder(passer_player_name,avg_epa)))+
  geom_bar(aes(fill= team_color, color=team_color2),stat = "identity")+
  scale_color_identity(aesthetics = c("fill","color"))+
  theme_bw() +
  labs(x= "Third Down EPA/Play",
       y= "Passer Name",
       title= "Who are the Best QBs on Third Down",
       subtitle="2024, minimum of 60 passes")+
  theme(legend.position="none",
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12,hjust=0.5),
        panel.grid.major.y = element_line(size=0.1))

#######################################
pbp_punt24 <- load_pbp(2024)
pbp_punt <- pbp_punt24 |>
  filter(play_type == "punt")|>
  filter(yardline_100 - kick_distance >= 1)
  # and get plays that have epa
  filter(!is.na(epa))

punt_eff <- pbp_punt %>%
  #filter(posteam = ()) %>%
  filter(yardline_100 <= 60) %>%
  filter(yardline_100 >= 40) %>%
  filter(kick_distance - yardline_100 <= 0) %>%
  filter(!is.na(punter_player_name)) %>%
  group_by(punter_player_name) %>%
  summarize(punts = n(),
            pin_eff = mean(yardline_100) - mean(kick_distance),
            team_abbr= last(posteam)) %>%
  filter(punts >= 15) %>%
  left_join(teams_colors_logos, by = "team_abbr")
arrange(-punt_eff)

punt_eff %>%
  ggplot(aes(x= pin_eff, y= fct_reorder(punter_player_name,-pin_eff)))+
  geom_bar(aes(fill= team_color, color=team_color2),stat = "identity")+
  scale_color_identity(aesthetics = c("fill","color"))+
  theme_bw() +
  labs(x= "Avg Starting Position",
       y= "Punter Name",
       title= "Who's the best Punter at pinning teams deep?",
       caption= "By Connor Dague | @cdague10",
       subtitle="2024, minimum of 20 pooch punts")+
  theme(legend.position="none",
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12,hjust=0.5),
        panel.grid.major.y = element_line(size=0.1))
