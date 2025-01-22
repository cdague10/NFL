library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)
library(ggimage)
library(dplyr)
library(gt)

#Combine ftn and pbp together
ftn <- load_ftn_charting(season = 2024)
ftn <- ftn %>% dplyr::select(nflverse_game_id,ftn_play_id:is_qb_fault_sack) %>%
  dplyr::rename(game_id = nflverse_game_id, play_id = nflverse_play_id)
pbp <- load_pbp(2024)
pbp <- left_join(nfl_pbp,ftn,by=c("game_id","play_id")) %>%
  dplyr::filter(!is.na(ftn_play_id))

#Late down qb location running
late_down_run <- pbp %>%
  filter(qb_dropback == 0) %>%
  filter(play_type == "run") %>%
  filter(down >= 3 | goal_to_go <= 2) %>%
  filter(ydstogo <= 2) %>%
  group_by(qb_location)

shotgun_run_total_l <- sum(late_down_run$qb_location == "S"| late_down_run$qb_location == "P")
shotgun_run_success_l <- sum((late_down_run$qb_location == "S"|late_down_run$qb_location == "P")& late_down_run$success == 1)
shotgun_run_successrate_l <- shotgun_run_success_l/shotgun_run_total_l

under_run_total_l <- sum(late_down_run$qb_location == "U")
under_run_success_l <- sum(late_down_run$qb_location == "U" & late_down_run$success == 1)
under_run_successrate_l <- under_run_success_l/under_run_total_l
######################################################
#Early down Qb location running
early_down_run <- pbp %>%
  filter(qb_dropback == 0) %>%
  filter(play_type == "run") %>%
  filter(down <= 2) %>%
  group_by(qb_location)

shotgun_run_total_e <- sum(early_down_run$qb_location == "S"| early_down_run$qb_location == "P")
shotgun_run_success_e <- sum((early_down_run$qb_location == "S"|early_down_run$qb_location == "P")& early_down_run$yards_gained >= 4)
shotgun_run_successrate_e <- shotgun_run_success_e/shotgun_run_total_e

under_run_total_e <- sum(early_down_run$qb_location == "U")
under_run_success_e <- sum(early_down_run$qb_location == "U" & early_down_run$yards_gained >= 4)
under_run_successrate_e <- under_run_success_e/under_run_total_e

run_type <- c(shotgun_run_successrate_l,under_run_successrate_l,shotgun_run_successrate_e,under_run_successrate_e)


combined_summary <- tibble(
  qb_location = c("Shotgun/Pistol", "Under Center", "Shotgun/Pistol","Under Center"),
  success_rate = round(c(shotgun_run_successrate_l,under_run_successrate_l,shotgun_run_successrate_e,under_run_successrate_e),2),
  down_type = c("  Late Down", "1-2 yds to go","  Early Down","4+ yds on 1st")
)


gt_table <- combined_summary %>%
  mutate(success_rate = paste0(round(c(shotgun_run_successrate_l, under_run_successrate_l, shotgun_run_successrate_e, under_run_successrate_e)*100, 2), "%"))%>%
  gt() %>%
  cols_label(
    qb_location = "Quarterback Location",
    success_rate = "Success Rate (%)",
    down_type = "Situation"
  ) %>%
  tab_header(
    title = "Run Success by Formation and Situation",
    subtitle = "2024 season | @cdague10"
  ) %>%
  tab_spanner(
    label = "Rushing Plays",
    columns = c("success_rate","down_type")
  ) %>%
  tab_options(
    table.background.color = "#f9f9f9",
    table.border.top.style = "solid",
    table.border.top.width = 2,
    table.border.bottom.style = "solid",
    table.border.bottom.width = 2,
    table.border.right.style = "solid",
    table.border.right.width = 2,
    table.border.left.style = "solid",
    table.border.left.width = 2,
    column_labels.background.color = "#4CAF50",
    column_labels.font.weight = "bold",
    column_labels.font.size = 14,
    column_labels.border.top.style = "solid",
    column_labels.border.top.width = 2,
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f2f2f2"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "grey",
      weight = px(2)
    ),
    locations = cells_body(columns = c("success_rate","down_type"))
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "grey",
      weight = px(2)
    ),
    locations = cells_body(columns = ("success_rate"))
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "grey",
      weight = px(4)
    ),
    locations = cells_body(rows = 2)
  )

# Display the table
gt_table

########################################################################33
off_motion <- pbp %>%
  filter(is_motion == "TRUE") %>%
  group_by(posteam)%>%
  summarise(off_epa = mean(epa))
no_off_motion <- pbp %>%
  filter(is_motion == "FALSE") %>%
  filter(posteam != is.na(posteam)) %>%
  group_by(posteam)%>%
  summarise(off_no_epa = mean(epa))
def_motion <- pbp%>%
  filter(is_motion == "TRUE")%>%
  filter(defteam != is.na(defteam))%>%
  group_by(defteam) %>%
  summarise(def_epa = mean(epa))
no_def_motion <- pbp %>%
  filter(is_motion == "FALSE") %>%
  filter(defteam != is.na(defteam)) %>%
  group_by(defteam)%>%
  summarise(def_no_epa = mean(epa))

motion_epa <- off_motion %>%
  left_join(no_off_motion, by = "posteam") %>%
  rename(defteam = posteam) %>%
  left_join(def_motion, by = "defteam") %>%
  left_join(no_def_motion, by = "defteam") %>%
  left_join(teams_colors_logos, by = "defteam")

motion_epa <- off_motion %>%
  left_join(no_off_motion, by = "posteam") %>%
  rename(defteam = posteam) %>%
  left_join(def_motion, by = "defteam") %>%
  left_join(no_def_motion, by = "defteam") %>%
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr")) %>%
  mutate(off_motion_diff = off_epa - off_no_epa,
         def_motion_diff = def_epa - def_no_epa)


motion_epa %>%
  ggplot(aes(x= off_epa, y= fct_reorder(defteam,off_epa)))+
  geom_bar(aes(fill= team_color, color=team_color2),stat = "identity")+
  scale_color_identity(aesthetics = c("fill","color"))+
  theme_bw() +
  labs(x= "EPA/Play",
       y= "Teams",
       title= "Best NFL Offenses with Motion ",
       subtitle="2024 season, EPA/Plays with Motion")+
  theme(legend.position="none",
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12,hjust=0.5),
        panel.grid.major.y = element_line(size=0.1))
