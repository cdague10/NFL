library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(stringr)
library(xgboost)
library(caret) # for train/test split and performance

pbp <- load_pbp(2024)

pbp_clean <- pbp %>%
  filter(play_type %in% c("run", "pass"), !is.na(rush_attempt), !is.na(qb_dropback)) %>%
  mutate(
    is_rush = as.integer(play_type == "run"),
    shotgun = ifelse(is.na(shotgun), 0, shotgun),
    no_huddle = ifelse(is.na(no_huddle), 0, no_huddle),
    qtr = as.factor(qtr),
    down = as.factor(down),
    ydstogo = log1p(ydstogo),  # log transform to smooth it
    score_diff = posteam_score - defteam_score
  ) %>%
  select(posteam, is_rush, down, ydstogo, qtr, shotgun, no_huddle, score_diff, half_seconds_remaining, game_seconds_remaining)

set.seed(123)
split <- initial_split(pbp_clean, prop = 0.8)
train <- training(split)
test <- testing(split)

# Train a logistic regression model
rush_model <- glm(is_rush ~ down + ydstogo + qtr + shotgun + no_huddle + score_diff,
                  data = train, family = "binomial")

# Predict expected rush probabilities
test <- test %>%
  mutate(expected_rush_prob = predict(rush_model, newdata = test, type = "response"))

rroe_team <- test %>%
  group_by(posteam) %>%
  summarise(
    actual_rush_rate = mean(is_rush),
    expected_rush_rate = mean(expected_rush_prob),
    RROE = actual_rush_rate - expected_rush_rate
  ) %>%
  arrange(desc(RROE))








pbp_clean <- pbp %>%
  filter(play_type %in% c("run", "pass"), !is.na(rush_attempt), !is.na(qb_dropback)) %>%
  mutate(
    is_rush = as.integer(play_type == "run"),
    shotgun = ifelse(is.na(shotgun), 0, shotgun),
    no_huddle = ifelse(is.na(no_huddle), 0, no_huddle),
    qtr = as.factor(qtr),
    down = as.factor(down),
    ydstogo = log1p(ydstogo),  # log transform to smooth it
    score_diff = posteam_score - defteam_score
  ) %>%
  select(posteam, is_rush, down, ydstogo, qtr, shotgun, no_huddle, score_diff, half_seconds_remaining, game_seconds_remaining)

#Prepare data for Xgboost
# One-hot encode factors
dummies <- dummyVars(~ down + qtr, data = pbp_clean)
dummy_mat <- predict(dummies, pbp_clean)

# Combine numeric and dummy variables
X <- cbind(
  dummy_mat,
  ydstogo = pbp_clean$ydstogo,
  shotgun = pbp_clean$shotgun,
  no_huddle = pbp_clean$no_huddle,
  score_diff = pbp_clean$score_diff,
  half_seconds_remaining = pbp_clean$half_seconds_remaining,
  game_seconds_remaining = pbp_clean$game_seconds_remaining
)

y <- pbp_clean$is_rush
posteam <- pbp_clean$posteam

#Train/Test Split
set.seed(123)
train_idx <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_idx, ]
X_test <- X[-train_idx, ]
y_train <- y[train_idx]
y_test <- y[-train_idx]
posteam_test <- posteam[-train_idx]


#Train XgBoost Model
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain),
  verbose = 0
)

#Calculate Expected RR
expected_rush_prob <- predict(xgb_model, dtest)

#Calculate RROE
results <- tibble(
  posteam = posteam_test,
  is_rush = y_test,
  expected_rush_prob = expected_rush_prob
)

rroe_team <- results %>%
  group_by(posteam) %>%
  summarise(
    actual_rush_rate = mean(is_rush),
    expected_rush_rate = mean(expected_rush_prob),
    RROE = actual_rush_rate - expected_rush_rate,
    n_plays = n()
  ) %>%
  arrange(desc(RROE))
