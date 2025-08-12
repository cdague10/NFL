library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(stringr)
library(nflreadr)
drafted_players <- load_draft_picks(2005:2020)

RB_clean <- drafted_players %>%
  filter(position == "WR",pick <= 75, rec_yards <= 12500, !is.na(rec_yards), rec_yards > 0)

ggplot(RB_clean, aes(x = pick, y = rec_yards)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_fill_viridis_c(
    option = "C",
    labels = scales::label_number(accuracy = 0.0000001),
    name = "Density"
  ) +
  labs(
    title = "Career Receiving Yards vs Draft Pick (Drafted 2010-2020)",
    caption = "Data: NFLreadR | By: Connor Dague                                                                                    ",
    x = "Draft Pick Number (Overall)",
    y = "Receiving Yards"
  ) +
  theme_minimal()

WR_clean_dist <- drafted_players %>%
  filter(position == "WR",pick <= 75, rec_yards >= 5000, !is.na(rec_yards), rec_yards > 0)

ggplot(WR_clean_dist, aes(x = pick)) +
  geom_density(fill = "skyblue", color = "white", boundary = 0) +
  labs(
    title = "Distribution of 5,000+ Yard WRs by Draft Pick",
    subtitle = paste("N =", nrow(WR_clean_dist), "WRs since", min(WR_clean_dist$season, na.rm = TRUE)),
    x = "Draft Pick Number (Overall)",
    y = "Density"
  ) +
  theme_minimal()
