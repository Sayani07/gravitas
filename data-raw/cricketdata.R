library(readr)
library(tsibble)
library(dplyr)

cricket <- read_csv("data-raw/deliveries.csv")

# each over needs to have 6 balls

cricket_fltr_ball <- cricket %>%
  filter(wide_runs + noball_runs == 0) %>% # filtering out scores that are gained by no balls or wide
  # still have to filter out overs for which number of balls per over less than 6
  group_by(batting_team, match_id, inning, over) %>%
  summarise(n_over = length(ball)) %>%
  filter(n_over == 6) %>%
  mutate(key = paste(batting_team, match_id, inning, over, sep = "_"))


cricket_over_crctd <- cricket %>%
  mutate(cricket_key = paste(batting_team, match_id, inning, over, sep = "_")) %>%
  filter(cricket_key %in% cricket_fltr_ball$key) %>%
  filter(wide_runs + noball_runs == 0) %>%
  mutate(ball_per_over = purrr::rep_along(match_id, 1:6))

# each innings need to have 20 overs

cricket_fltr_over <- cricket_over_crctd %>%
  filter(inning %in% c(1, 2)) %>%
  group_by(inning, match_id) %>%
  summarise(n = length(over)) %>%
  filter(n == 120) %>%
  mutate(key = paste(match_id, inning, sep = "_"))

cricket_over_inning_crctd <- cricket_over_crctd %>%
  mutate(over_key = paste(match_id, inning, sep = "_")) %>%
  filter(over_key %in% cricket_fltr_over$key) %>%
  filter(wide_runs + noball_runs == 0) %>%
  mutate(ball_per_over = purrr::rep_along(match_id, 1:6)) %>%
  filter(inning %in% c(1, 2))


# each match needs to have 2 innings

cricket_filtr_inning <- cricketdata %>% group_by(match_id) %>% summarise(length(inning)) %>% filter(`length(inning)` == 240)

cricketdata <- cricket_over_inning_crctd %>%
  filter(match_id %in% cricket_filtr_inning$match_id) %>%
  filter(wide_runs + noball_runs == 0) %>%
  mutate(ball_per_over = purrr::rep_along(match_id, 1:6)) %>%
  filter(inning %in% c(1, 2))

save(cricketdata, file = "data/cricketdata.rda")
