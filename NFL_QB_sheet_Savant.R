library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(stringr)

pbp <- load_pbp(2024)

#QB Overall EPA Breakdown 2024
qb_epa<- pbp %>%
  filter(posteam != "dhs", !is.na(id)) %>%
  group_by(name)%>%
  summarize(
    name = first(name),
    team_abbr = last(posteam),
    plays = n(),
    epa_per_play = mean(epa),
    air_yds = mean(air_yards),
    pass_attempts = sum(complete_pass + incomplete_pass)) %>%
  filter(pass_attempts >= 5) %>%
  mutate(name = str_replace(name, "^(\\w)\\w*\\s+", "\\1."))%>%
  left_join(teams_colors_logos, by = "team_abbr")

#PFF Stats- All 2024 Passing Stats
passing_summary <- passing_summary %>%
  filter(position == "QB",
         attempts >= 120) %>%
  mutate(
    accuracy_percentile = percent_rank(accuracy_percent)*100,
    btt_percentile = percent_rank(btt_rate)*100,
    pts_percentile = percent_rank(-pressure_to_sack_rate)*100,
    twp_percentile = percent_rank(-twp_rate)*100,
    adot_percentile = percent_rank(avg_depth_of_target)*100,
    passgrade_percentile = percent_rank(grades_pass)*100,
    rungrade_percentile = percent_rank(grades_run)*100,
  )
  
selected_qb <- passing_summary %>%
  filter(player == "Russell Wilson") #input QB's name


percentiles <- tribble(
  ~Metric,                    ~Percentile,
  "PFF Pass Grade",            round(selected_qb$passgrade_percentile),
  "PFF Run Grade",             round(selected_qb$rungrade_percentile),
  "Accuracy Percent",          round(selected_qb$accuracy_percentile),
  "Average Depth of Target",   round(selected_qb$adot_percentile),
  "Big Time Throw Rate",       round(selected_qb$btt_percentile),
  "Pressure-To-Sack Rate",     round(selected_qb$pts_percentile),
  "Turnover-Worthy Play Rate", round(selected_qb$twp_percentile)
) %>%
  mutate(Metric = factor(Metric, levels = rev(Metric)))

plot_percentiles <- percentiles %>%
  ggplot(aes(x = Metric, y = Percentile, fill = Percentile)) +  # fill mapped to Percentile
  geom_col(width = 0.9) +  # no manual fill here
  
  # Circle at end of bar
  geom_point(aes(y = Percentile), color = "white", fill = "darkgrey",
             shape = 21, size = 15, stroke = 1.2) +
  
  # Percentile text in the circle
  geom_text(
    aes(y = Percentile, label = Percentile),
    color = "white", size = 4.5, fontface = "bold",
    hjust = 0.5, vjust = 0.3
  ) + 
  coord_flip() +
  labs(title = "2024 NFL QB Statistics", x = NULL, y = "Percentile") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_gradient(low = "red", high = "green") +  # red = low, green = high
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 15),
    legend.position = "none"
  )

# Display the plot
plot_percentiles

qb_sum <- selected_qb %>%
  select(player, grades_pass, grades_run, accuracy_percent, avg_depth_of_target,
         btt_rate, pressure_to_sack_rate, twp_rate)
