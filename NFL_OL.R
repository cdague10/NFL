library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

offense_blocking <- offense_blocking %>%           #run each line of code one by one
  #left_join(offense_pass_blocking, by = "player")
  #left_join(offense_run_blockng, by = "player")
  #left_join(NFL_logos_colors, by = "team_name")
  #select(!ends_with(".y"))
  select(!ends_with(".x"))

position_snaps <- offense_blocking %>%
  group_by(team_name) %>%
  summarize(
    total_lt = sum(snap_counts_lt, na.rm = TRUE),
    total_lg = sum(snap_counts_lg, na.rm = TRUE),
    total_ce = sum(snap_counts_ce, na.rm = TRUE),
    total_rg = sum(snap_counts_rg, na.rm = TRUE),
    total_rt = sum(snap_counts_rt, na.rm = TRUE))

offense_blocking <- offense_blocking %>%
  left_join(position_snaps, by = "team_name")

offense_blocking <- offense_blocking %>%
  filter(position %in% c("T","G","C")) %>%
  filter(snap_counts_offense >= 5)%>%
  group_by(team_name, player) %>%
  slice_max(order_by = snap_counts_offense, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(lt_grade = grades_offense*(snap_counts_lt/total_lt),
         lg_grade = grades_offense*(snap_counts_lg/total_lg),
         ce_grade = grades_offense*(snap_counts_ce/total_ce),
         rg_grade = grades_offense*(snap_counts_rg/total_rg),
         rt_grade = grades_offense*(snap_counts_rt/total_rt))

team_ol_grades <- offense_blocking %>%
  group_by(team_name) %>%
  summarize(
    lt_grade = sum(lt_grade, na.rm = TRUE),
    lg_grade = sum(lg_grade, na.rm = TRUE),
    ce_grade = sum(ce_grade, na.rm = TRUE),
    rg_grade = sum(rg_grade, na.rm = TRUE),
    rt_grade = sum(rt_grade, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(overall_ol_grade = (lt_grade + lg_grade + ce_grade + rg_grade + rt_grade) / 5)%>%
  left_join(NFL_logos_colors, by = "team_name")

ol_table <- team_ol_grades %>%
  arrange(desc(overall_ol_grade)) %>%#this needs to run before everything else does
  select(
    team_logo_wiki,
    lt_grade,
    lg_grade,
    ce_grade,
    rg_grade,
    rt_grade
  ) %>%
  gt() %>%
  tab_header(
    title = "Top Offensive Lines in 2024",
    subtitle = "Weighted Average by Position | Stats: PFF | By: Connor Dague"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = px(18)),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(size = px(18)),
    locations = cells_body()
  ) %>%
  text_transform(
    locations = cells_body(columns = team_logo_wiki),
    fn = function(x) {
      web_image(url = x, height = 20)  # adjust height if needed
    }
  ) %>%
  cols_align(
    align = "center",
    columns = c(team_logo_wiki,lt_grade,lg_grade,ce_grade,rg_grade,rt_grade)
  ) %>%
  cols_label(
    team_logo_wiki = "Team",
    lt_grade = "LT",
    lg_grade = "LG",
    ce_grade = "Center",
    rg_grade = "RG",
    rt_grade = "RT"
  ) %>%
  fmt_number(
    columns = c(lt_grade, lg_grade, ce_grade, rg_grade, rt_grade),
    decimals = 1
  ) %>%
  cols_width(
    team_logo_wiki ~ px(30),
  ) %>%
  data_color(
    columns = c(lt_grade,lg_grade,ce_grade,rg_grade,rt_grade),
    colors = scales::col_numeric(
      palette = c("blue", "white", "green"),
      domain = NULL
    )
  )

ol_table









left_half <- team_ol_grades %>%
  arrange(desc(overall_ol_grade)) %>%
  slice(1:16) %>%
  select(team_logo_wiki, lt_grade, lg_grade, ce_grade, rg_grade, rt_grade)

right_half <- team_ol_grades %>%
  arrange(desc(overall_ol_grade)) %>%
  slice(17:32) %>%
  select(team_logo_wiki, lt_grade, lg_grade, ce_grade, rg_grade, rt_grade) %>%
  rename_with(~ paste0(., "_r"), everything())

# Combine the two sides
split_table <- bind_cols(left_half, right_half)

# Create gt table
gt_table <- split_table %>%
  gt() %>%

  # Titles
  tab_header(
    title = md("**Top Offensive Lines in 2024 (Split View)**"),
    subtitle = md("Weighted Average by Position | Stats: PFF | By: Connor Dague")
  ) %>%

  # Style title and body
  tab_style(
    style = cell_text(weight = "bold", size = px(18)),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = cell_text(size = px(16)),
    locations = cells_body()
  ) %>%

  # Team logos on both sides
  text_transform(
    locations = cells_body(columns = team_logo_wiki),
    fn = function(x) {
      web_image(url = x, height = 20)
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = team_logo_wiki_r),
    fn = function(x) {
      web_image(url = x, height = 20)
    }
  ) %>%

  # Column alignment
  cols_align(
    align = "center",
    columns = everything()
  ) %>%

  # Remove header text from logo columns
  cols_label(
    team_logo_wiki = "",
    lt_grade = "LT", lg_grade = "LG", ce_grade = "C", rg_grade = "RG", rt_grade = "RT",
    team_logo_wiki_r = "",
    lt_grade_r = "LT", lg_grade_r = "LG", ce_grade_r = "C", rg_grade_r = "RG", rt_grade_r = "RT"
  ) %>%

  # Column widths
  cols_width(
    starts_with("team_logo_wiki") ~ px(70),
    starts_with("lt") ~ px(55),
    starts_with("lg") ~ px(55),
    starts_with("ce") ~ px(55),
    starts_with("rg") ~ px(55),
    starts_with("rt") ~ px(55)
  ) %>%

  # Number formatting
  fmt_number(
    columns = matches("grade"),
    decimals = 1
  ) %>%

  # Color scale
  data_color(
      columns = matches("grade"),
      colors = scales::col_numeric(
        palette = c("blue", "white", "green"),
        domain = c(44,93)  # or tweak to your real min/max
      )
    )

gt_table
