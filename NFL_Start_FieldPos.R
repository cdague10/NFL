install.packages("tidymodels")
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(nfl4th)
#view(teams_colors_logos)
library(tidyverse)
library(tidymodels)

pbp <- load_pbp(2004:2024)
names(pbp)

pbp_2pt <- pbp %>%
  filter(!is.na(play_type)) %>%
  filter(play_type != "kickoff") %>%
  filter(two_point_attempt == 1) %>%
  filter(score_differential >= 8) %>%
  filter(two_point_conv_result == "failure") #success or failure
  #filter(pass == 0 | rush == 0) %>%
  # and get plays that have epa
  #filter(!is.na(epa))

nrow(pbp_2pt)
#Down by 19 or more- 44 good, 35 fail
#Down 11 to 18 points- 86 good, 110 fail
#Down 10 to 3 points- 148 good, 163 fail
#Down 2 or 1 points (tie or take lead)- 93 good, 104 fail
#Up by 7 points or less- 170 good, 167 fail
#up by 8 or more- 93 good, 106 fail

################################################################
start_field_pos <- pbp %>%
  filter(down == 1) %>%
  group_by(game_id, drive) %>%  # Group by game and drive
  filter(row_number() == 1) %>%
  filter(play_type != "qb_kneel")

block1 <- start_field_pos %>%
  filter(yardline_100 >=95) %>%
  filter(yardline_100 <= 99)
block2 <- start_field_pos %>%
  filter(yardline_100 >=90) %>%
  filter(yardline_100 <= 94)
block3 <- start_field_pos %>%
  filter(yardline_100 >=85) %>%
  filter(yardline_100 <= 89)
block4 <- start_field_pos %>%
  filter(yardline_100 >=80) %>%
  filter(yardline_100 <= 84)
block5 <- start_field_pos %>%
  filter(yardline_100 >=75) %>%
  filter(yardline_100 <= 79)
block6 <- start_field_pos %>%
  filter(yardline_100 >=70) %>%
  filter(yardline_100 <= 74)
block7 <- start_field_pos %>%
  filter(yardline_100 >=65) %>%
  filter(yardline_100 <= 69)
block8 <- start_field_pos %>%
  filter(yardline_100 >=60) %>%
  filter(yardline_100 <= 64)
block9 <- start_field_pos %>%
  filter(yardline_100 >=55) %>%
  filter(yardline_100 <= 59)
block10 <- start_field_pos %>%
  filter(yardline_100 >=50) %>%
  filter(yardline_100 <= 54)
block11 <- start_field_pos %>%
  filter(yardline_100 >=45) %>%
  filter(yardline_100 <= 49)
block12 <- start_field_pos %>%
  filter(yardline_100 >=40) %>%
  filter(yardline_100 <= 44)
block13 <- start_field_pos %>%
  filter(yardline_100 >=35) %>%
  filter(yardline_100 <= 39)
block14 <- start_field_pos %>%
  filter(yardline_100 >=30) %>%
  filter(yardline_100 <= 34)
block15 <- start_field_pos %>%
  filter(yardline_100 >=25) %>%
  filter(yardline_100 <= 29)
block16 <- start_field_pos %>%
  filter(yardline_100 >=20) %>%
  filter(yardline_100 <= 24)
block17 <- start_field_pos %>%
  filter(yardline_100 >=15) %>%
  filter(yardline_100 <= 19)
block18 <- start_field_pos %>%
  filter(yardline_100 >=10) %>%
  filter(yardline_100 <= 14)
block19 <- start_field_pos %>%
  filter(yardline_100 >=5) %>%
  filter(yardline_100 <= 9)
block20 <- start_field_pos %>%
  filter(yardline_100 >=1) %>%
  filter(yardline_100 <= 4)

numdrives1 <- nrow(block1)
td1 <- (sum(block1$drive_end_transition == "TOUCHDOWN")/numdrives1)
fg1 <- (sum(block1$drive_end_transition == "FIELD_GOAL")/numdrives1)
play1 <- mean(block1$drive_play_count)
eckel1 <- (sum(block1$yardline_100 - block1$ydsnet <= 35)/numdrives1)
expoint1 <- (td1*6.96)+(fg1*3)

numdrives2 <- nrow(block2)
td2 <- (sum(block2$drive_end_transition == "TOUCHDOWN")/numdrives2)
fg2 <- (sum(block2$drive_end_transition == "FIELD_GOAL")/numdrives2)
play2 <- mean(block2$drive_play_count)
eckel2 <- (sum(block2$yardline_100 - block2$ydsnet <= 35)/numdrives2)
expoint2 <- (td2*6.96)+(fg2*3)

numdrives3 <- nrow(block3)
td3 <- (sum(block3$drive_end_transition == "TOUCHDOWN")/numdrives3)
fg3 <- (sum(block3$drive_end_transition == "FIELD_GOAL")/numdrives3)
play3 <- mean(block3$drive_play_count)
eckel3 <- (sum(block3$yardline_100 - block3$ydsnet <= 35)/numdrives3)
expoint3 <- (td3*6.96)+(fg3*3)

numdrives4 <- nrow(block4)
td4 <- (sum(block4$fixed_drive_result == "Touchdown")/numdrives4)
fg4 <- (sum(block4$fixed_drive_result == "Field goal")/numdrives4)
play4 <- mean(block4$drive_play_count)
eckel4 <- (sum(block4$yardline_100 - block4$ydsnet <= 35)/numdrives4)
expoint4 <- (td4*6.96)+(fg4*3)

numdrives5 <- nrow(block5)
td5 <- (sum(block5$fixed_drive_result == "Touchdown")/numdrives5)
fg5 <- (sum(block5$fixed_drive_result == "Field goal")/numdrives5)
play5 <- mean(block5$drive_play_count)
eckel5 <- (sum(block5$yardline_100 - block5$ydsnet <= 35)/numdrives5)
expoint5 <- (td5*6.96)+(fg5*3)

numdrives6 <- nrow(block6)
td6 <- (sum(block6$drive_end_transition == "TOUCHDOWN")/numdrives6)
fg6 <- (sum(block6$drive_end_transition == "FIELD_GOAL")/numdrives6)
play6 <- mean(block6$drive_play_count)
eckel6 <- (sum(block6$yardline_100 - block6$ydsnet <= 35)/numdrives6)
expoint6 <- (td6*6.96)+(fg6*3)

numdrives7 <- nrow(block7)
td7 <- (sum(block7$drive_end_transition == "TOUCHDOWN")/numdrives7)
fg7 <- (sum(block7$drive_end_transition == "FIELD_GOAL")/numdrives7)
play7 <- mean(block7$drive_play_count)
eckel7 <- (sum(block7$yardline_100 - block7$ydsnet <= 35)/numdrives7)
expoint7 <- (td7*6.96)+(fg7*3)

numdrives8 <- nrow(block8)
td8 <- (sum(block8$drive_end_transition == "TOUCHDOWN")/numdrives8)
fg8 <- (sum(block8$drive_end_transition == "FIELD_GOAL")/numdrives8)
play8 <- mean(block8$drive_play_count)
eckel8 <- (sum(block8$yardline_100 - block8$ydsnet <= 35)/numdrives8)
expoint8 <- (td8*6.96)+(fg8*3)

numdrives9 <- nrow(block9)
td9 <- (sum(block9$fixed_drive_result == "Touchdown")/numdrives9)
fg9 <- (sum(block9$fixed_drive_result == "Field goal")/numdrives9)
play9 <- mean(block9$drive_play_count)
eckel9 <- (sum(block9$yardline_100 - block9$ydsnet <= 35)/numdrives9)
expoint9 <- (td9*6.96)+(fg9*3)

numdrives10 <- nrow(block10)
td10 <- (sum(block10$drive_end_transition == "TOUCHDOWN")/numdrives10)
fg10 <- (sum(block10$drive_end_transition == "FIELD_GOAL")/numdrives10)
play10 <- mean(block10$drive_play_count)
eckel10 <- (sum(block10$yardline_100 - block10$ydsnet <= 35)/numdrives10)
expoint10 <- (td10*6.96)+(fg10*3)

numdrives11 <- nrow(block11)
td11 <- (sum(block11$fixed_drive_result == "Touchdown")/numdrives11)
fg11 <- (sum(block11$fixed_drive_result == "Field goal")/numdrives11)
play11 <- mean(block11$drive_play_count)
eckel11 <- (sum(block11$yardline_100 - block11$ydsnet <= 35)/numdrives11)
expoint11 <- (td11*6.96)+(fg11*3)

numdrives12 <- nrow(block12)
td12 <- (sum(block12$drive_end_transition == "TOUCHDOWN")/numdrives12)
fg12 <- (sum(block12$drive_end_transition == "FIELD_GOAL")/numdrives12)
play12 <- mean(block12$drive_play_count)
eckel12 <- (sum(block12$series_success == 1)/numdrives12)
expoint12 <- (td12*6.96)+(fg12*3)

numdrives13 <- nrow(block13)
td13 <- (sum(block13$drive_end_transition == "TOUCHDOWN")/numdrives13)
fg13 <- (sum(block13$drive_end_transition == "FIELD_GOAL")/numdrives13)
play13 <- mean(block13$drive_play_count)
eckel13 <- (sum(block13$series_success == 1)/numdrives13)
expoint13 <- (td13*6.96)+(fg13*3)

numdrives14 <- nrow(block14)
td14 <- (sum(block14$fixed_drive_result == "Touchdown")/numdrives14)
fg14 <- (sum(block14$fixed_drive_result == "Field goal")/numdrives14)
play14 <- 4.724 #hand calculation
eckel14 <- (sum(block14$series_success ==1)/numdrives14)
expoint14 <- (td14*6.96)+(fg14*3)

numdrives15 <- nrow(block15)
td15 <- (sum(block15$fixed_drive_result == "Touchdown")/numdrives15)
fg15 <- (sum(block15$fixed_drive_result == "Field goal")/numdrives15)
play15 <- mean(block15$drive_play_count)
eckel15 <- (sum(block15$series_success == 1)/numdrives15)
expoint15 <- (td15*6.96)+(fg15*3)

numdrives16 <- nrow(block16)
td16 <- (sum(block16$drive_end_transition == "TOUCHDOWN")/numdrives16)
fg16 <- (sum(block16$drive_end_transition == "FIELD_GOAL")/numdrives16)
play16 <- mean(block16$drive_play_count)
eckel16 <- (sum(block16$series_success == 1)/numdrives16)
expoint16 <- (td16*6.96)+(fg16*3)

numdrives17 <- nrow(block17)
td17 <- (sum(block17$drive_end_transition == "TOUCHDOWN")/numdrives17)
fg17 <- (sum(block17$drive_end_transition == "FIELD_GOAL")/numdrives17)
play17 <- mean(block17$drive_play_count)
eckel17 <- (sum(block17$series_success == 1)/numdrives17)
expoint17 <- (td17*6.96)+(fg17*3)

numdrives18 <- nrow(block18)
td18 <- (sum(block18$drive_end_transition == "TOUCHDOWN")/numdrives18)
fg18 <- (sum(block18$drive_end_transition == "FIELD_GOAL")/numdrives18)
play18 <- mean(block18$drive_play_count)
eckel18 <- (sum(block18$series_success == 1)/numdrives18)
expoint18 <- (td18*6.96)+(fg18*3)

numdrives19 <- nrow(block19)
td19 <- (sum(block19$drive_end_transition == "TOUCHDOWN")/numdrives19)
fg19 <- (sum(block19$drive_end_transition == "FIELD_GOAL")/numdrives19)
play19 <- mean(block19$drive_play_count)
eckel19 <- (sum(block19$series_success == 1)/numdrives19)
expoint19 <- (td19*6.96)+(fg19*3)

numdrives20 <- nrow(block20)
td20 <- (sum(block20$drive_end_transition == "TOUCHDOWN")/numdrives20)
fg20 <- (sum(block20$drive_end_transition == "FIELD_GOAL")/numdrives20)
play20 <- mean(block20$drive_play_count)
eckel20 <- (sum(block20$series_success == 1)/numdrives20)
expoint20 <- (td20*6.96)+(fg20*3)

###############################################
fg = c(fg1,fg2,fg3,fg4,fg5,fg6,fg7,fg8,fg9,fg10,fg11,fg12,fg13,fg14,fg15,fg16,fg17,fg18,fg19,fg20)
fg_percent <- paste0(round(fg * 100, 2), "%")
td = c(td1,td2,td3,td4,td5,td6,td7,td8,td9,td10,td11,td12,td13,td14,td15,td16,td17,td18,td19,td20)
td_percent <- paste0(round(td*100,2), "%")
eckel = c(eckel1,eckel2,eckel3,eckel4,eckel5,eckel6,eckel7,eckel8,eckel9,eckel10,eckel11,eckel12,eckel13,eckel14,eckel15,eckel16,eckel7,eckel8,eckel9,eckel20)
eckel_percent <- paste0(round(eckel*100,2),"%")
expected_points2 = c(expoint1,expoint2,expoint3,expoint4,expoint5,expoint6,expoint7,expoint8,expoint9,expoint10,expoint11,expoint12,expoint13,expoint14,expoint15,expoint16,expoint17,expoint18,expoint19,expoint20)
expected_points <- paste0(round(expected_points2,2))
num_plays2 = c(play1,play2,play3,play4,play5,play6,play7,play8,play9,play10,play11,play12,play13,play14,play15,play16,play17,play18,play19,play20)
num_plays <- paste0(round(num_plays2,2))
###################################################################

data <- data.frame(
  RowLabel = c("Inside own 5", "Inside own 6-10", "Inside own 11-15", "Inside own 16-20",
               "Inside own 21-25", "Inside own 26-30", "Inside own 31-35", "Inside own 36-40",
               "Inside own 41-45", "Inside own 46-50", "Opposing 49-45", "Opposing 44-40",
               "Opposing 39-35", "Opposing 34-30", "Opposing 29-25", "Opposing 24-20",
               "Opposing 19-15", "Opposing 14-10", "Opposing 9-5", "Opposing 4-1"),
  block1 = expected_points,
  block2 = td_percent,
  block3 = fg_percent,
  block4 = eckel_percent,
  block5 = num_plays
)

# Create the gt table and apply custom options
gt_table <- gt(data) %>%
  cols_label(
    RowLabel = "Starting Field Position",  # Rename RowLabel column
    block1 = "Expected Points",
    block2 = "TD Percent",
    block3 = "FG Percent",
    block4 = "Eckel Percent",
    block5 = "Number of Plays"
  ) %>%
  tab_options(
    table.background.color = "#f9f9f9",  # Set the background color for the entire table
    table.border.top.style = "solid",    # Border style for the top of the table
    table.border.top.width = 2,          # Border width for the top of the table
    table.border.bottom.style = "solid", # Border style for the bottom of the table
    table.border.bottom.width = 2,       # Border width for the bottom of the table
    column_labels.background.color = "#4CAF50",  # Column header background color
    column_labels.font.weight = "bold",  # Make column labels bold
    column_labels.font.size = 14,       # Column labels font size
    row.striping.include_table_body = TRUE,  # Add striping to table rows
    row.striping.background_color = "#f2f2f2",  # Row striping color
  )

# Display the table
gt_table

