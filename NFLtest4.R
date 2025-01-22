library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(nfl4th)
#view(teams_colors_logos)
library(tidymodels)

pbp <- load_pbp(2024)

qb_throwlocation<- pbp |>
  filter(posteam != "dhs", !is.na(id)) |>
  filter(air_yards <= 5) |>
  group_by(name)|>
  summarize(
    name = first(name),
    team_abbr = last(posteam),
    plays = n(),
    epa_per_play = mean(epa),
    air_yds = mean(air_yards),
    pass_attempts = sum(complete_pass + incomplete_pass)
  ) |>
  filter(pass_attempts >= 175) |>
  left_join(teams_colors_logos, by = "team_abbr") |>
  arrange(-epa_per_play)

qb_throwlocation %>%
  ggplot(aes(x= epa_per_play, y= fct_reorder(name,epa_per_play)))+
  geom_bar(aes(fill= team_color, color=team_color2),stat = "identity")+
  scale_color_identity(aesthetics = c("fill","color"))+
  theme_bw() +
  labs(x= "EPA/Pass less than 5 air yards",
       y= "Passer Name",
       title= "Most efficient QBs throwing checkdowns",
       subtitle="2024, minimum of 40 passes")+
  theme(legend.position="none",
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12,hjust=0.5),
        panel.grid.major.y = element_line(size=0.1))

########################################################################
qb_shortgame<- pbp |>
  filter(posteam != "ush", !is.na(id)) |>
  filter(air_yards <= 10) |>
  group_by(name, pass_location) |>
  summarize(
    name = first(name),
    team_abbr = last(posteam),
    epa_per_play = mean(epa),
    air_yds = mean(air_yards),
    pass_attempts = sum(complete_pass + incomplete_pass)
  ) |>
  filter(pass_attempts >= 3) |>
  left_join(teams_colors_logos, by = "team_abbr") |>
  arrange(-epa_per_play)

qb_mediumgame<- pbp |>
  filter(posteam != "ush", !is.na(id)) |>
  filter(air_yards >=10,
         air_yards <= 20) |>
  group_by(name, pass_location) |>
  summarize(
    name = first(name),
    team_abbr = last(posteam),
    epa_per_play = mean(epa),
    air_yds = mean(air_yards),
    pass_attempts = sum(complete_pass + incomplete_pass)
  ) |>
  filter(pass_attempts >= 3) |>
  left_join(teams_colors_logos, by = "team_abbr") |>
  arrange(-epa_per_play)

qb_deepgame<- pbp |>
  filter(posteam != "ush", !is.na(id)) |>
  filter(air_yards >= 20) |>
  group_by(name, pass_location) |>
  summarize(
    name = first(name),
    team_abbr = last(posteam),
    epa_per_play = mean(epa),
    air_yds = mean(air_yards),
    pass_attempts = sum(complete_pass + incomplete_pass)
  ) |>
  filter(pass_attempts >= 1) |>
  left_join(teams_colors_logos, by = "team_abbr") |>
  arrange(-epa_per_play)

qb_combined_data <- rbind(qb_deepgame, qb_shortgame, qb_mediumgame)


#INPUT NEEDED
spec_qb_filter <- qb_combined_data %>%
  filter(name == "J.Burrow")



#Short pass  values
short_left <- (spec_qb_filter$pass_location == "left") & (spec_qb_filter$air_yds <= 9.99)
filtered_sl <- spec_qb_filter[short_left, ]
short_left_subset <- filtered_sl[, c("epa_per_play", "pass_location")]
short_left_epa <- filtered_sl$epa_per_play

short_mid <- (spec_qb_filter$pass_location == "middle") & (spec_qb_filter$air_yds <= 9.99)
filtered_sm <- spec_qb_filter[short_mid, ]
short_mid_subset <- filtered_sm[, c("epa_per_play", "pass_location")]
short_mid_epa <- filtered_sm$epa_per_play

short_right <- (spec_qb_filter$pass_location == "right") & (spec_qb_filter$air_yds <= 9.99)
filtered_sr <- spec_qb_filter[short_right, ]
short_right_subset <- filtered_sr[, c("epa_per_play", "pass_location")]
short_right_epa <- filtered_sr$epa_per_play

#Intermediate pass values
inter_left <- (spec_qb_filter$pass_location == "left") &
  (spec_qb_filter$air_yds <= 19.99 & spec_qb_filter$air_yds >= 10)
filtered_il <- spec_qb_filter[inter_left, ]
inter_left_subset <- filtered_il[, c("epa_per_play", "pass_location")]
inter_left_epa <- filtered_il$epa_per_play

inter_mid <- (spec_qb_filter$pass_location == "middle") &
  (spec_qb_filter$air_yds <= 19.99 & spec_qb_filter$air_yds >= 10)
filtered_im <- spec_qb_filter[inter_mid, ]
inter_mid_subset <- filtered_im[, c("epa_per_play", "pass_location")]
inter_mid_epa <- filtered_im$epa_per_play

inter_right <- (spec_qb_filter$pass_location == "right") &
  (spec_qb_filter$air_yds <= 19.99 & spec_qb_filter$air_yds >= 10)
filtered_ir <- spec_qb_filter[inter_right, ]
inter_right_subset <- filtered_ir[, c("epa_per_play", "pass_location")]
inter_right_epa <- filtered_ir$epa_per_play

#Deep pass  values
deep_left <- (spec_qb_filter$pass_location == "left") & (spec_qb_filter$air_yds >= 20)
filtered_dl <- spec_qb_filter[deep_left, ]
deep_left_subset <- filtered_dl[, c("epa_per_play", "pass_location")]
deep_left_epa <- filtered_dl$epa_per_play

deep_mid <- (spec_qb_filter$pass_location == "middle") & (spec_qb_filter$air_yds >= 20)
filtered_dm <- spec_qb_filter[deep_mid, ]
deep_mid_subset <- filtered_dm[, c("epa_per_play", "pass_location")]
deep_mid_epa <- filtered_dm$epa_per_play

deep_right <- (spec_qb_filter$pass_location == "right") & (spec_qb_filter$air_yds >= 20)
filtered_dr <- spec_qb_filter[deep_right, ]
deep_right_subset <- filtered_dr[, c("epa_per_play", "pass_location")]
deep_right_epa <- filtered_dr$epa_per_play

short_left_epa <- round(short_left_epa, 2)
short_mid_epa <- round(short_mid_epa, 2)
short_right_epa <- round(short_right_epa, 2)
inter_left_epa <- round(inter_left_epa, 2)
inter_mid_epa <- round(inter_mid_epa, 2)
inter_right_epa <- round(inter_right_epa, 2)
deep_left_epa <- round(deep_left_epa, 2)
deep_left_epa
deep_mid_epa <- round(deep_mid_epa, 2)
deep_right_epa <- round(deep_right_epa, 2)


average_epa <- sum(spec_qb_filter$epa_per_play *spec_qb_filter$pass_attempts)/sum(spec_qb_filter$pass_attempts)
footnote_text <- paste("The", spec_qb_filter$team_name[1], "QB averaged", round(average_epa, 2), "epa/play on", sum(spec_qb_filter$pass_attempts), "passes.")



data <- data.frame(
  RowLabel = c("Short (<10 yds)", "Intermediate (10-20 yds)", "Deep (>20 yds)"),
  block1 = c(short_right_epa, inter_right_epa, deep_right_epa),
  block2 = c(short_mid_epa, inter_mid_epa, deep_mid_epa),
  block3 = c(short_left_epa, inter_left_epa, deep_left_epa)
)


gt_table <- gt(data) %>%
  tab_header(
    title = "Quarterback Pass Attempt Data by Pass Location",
    subtitle = "EPA for short, intermediate, and deep passes by location"
  ) %>%
  cols_label(
    RowLabel = spec_qb_filter$name[1],  # Rename RowLabel column
    block1 = "Right",
    block2 = "Middle",
    block3 = "Left"
  ) %>%
  tab_options(
    table.background.color = "#f9f9f9",
    table.border.top.style = "solid",
    table.border.top.width = 2,
    table.border.bottom.style = "solid",
    table.border.bottom.width = 2,
    column_labels.background.color = spec_qb_filter$team_color[1],
    column_labels.font.weight = "bold",
    column_labels.font.size = 14,
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#f2f2f2"
  ) %>%

  cols_width(
    RowLabel ~ px(184),
    block1 ~ px(70),
    block2 ~ px(70),
    block3 ~ px(70)
  ) %>%
  tab_footnote(
    footnote = footnote_text
  )

# Display the table
gt_table
