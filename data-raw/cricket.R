library(readr)
library(tsibble)
library(dplyr)

#cricket <- read_csv("data-raw/deliveries.csv")


deliveries <-  read_csv("data-raw/deliveries_all.csv")
matches <- read_csv("data-raw/matches_all.csv")

cricket_season <- deliveries %>%
  left_join(matches, by = c("match_id" ="id")) %>%
  mutate(winner_team = if_else(winner == "batting_team", "batting_team","bowling_team"))


cricket_winner <- cricket_season %>% select(match_id, winner_team) %>% unique()
# making it uniform
# each over needs to have 6 balls

cricket_fltr_ball <- cricket_season %>%
  filter(wide_runs + noball_runs == 0) %>% # filtering out scores that are gained by no balls or wide
  # still have to filter out overs for which number of balls per over less than 6
  group_by(batting_team,
           match_id,
           inning,
           over) %>%
  summarise(n_over = length(ball)) %>%
  filter(n_over == 6) %>%
  mutate(key = paste(batting_team,
                     match_id,
                     inning,
                     over,
                     sep = "_"))


cricket_over_crctd <- cricket_season %>%
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
  filter(inning %in% c(1, 2))


# each match needs to have 2 innings

cricket_filtr_inning <- cricket_over_inning_crctd %>%
  group_by(match_id) %>%
  summarise(length(inning)) %>%
  filter(`length(inning)` == 240)

cricketdata <- cricket_over_inning_crctd %>%
  filter(match_id %in% cricket_filtr_inning$match_id) %>%
  filter(wide_runs + noball_runs == 0) %>%
  mutate(ball_per_over = purrr::rep_along(match_id, 1:6)) %>%
  filter(inning %in% c(1, 2))

#
# cricket <- cricketdata %>% select(season, match_id,
#                                   inning, batting_team,
#                                   bowling_team, ball_per_over,
#                                   over, winner, dismissal_kind,
#                                   total_runs)

# cricket <- cricketdata %>% group_by(season,
#                                     match_id,
#                                     batting_team,
#                                     bowling_team,
#                                     inning,
#                                     over) %>%
#   summarise(runs_per_over = sum(total_runs),
#             run_rate = sum(total_runs)/length(total_runs)) %>%
#   ungroup()


# cricket_tsibble_all <- cricket_per_over %>%
#   ungroup() %>%
#   mutate(data_index = row_number()) %>%
#   as_tsibble(index = data_index)


cricket <-  cricketdata %>%
  mutate(wicket = if_else(is.na(dismissal_kind),0,1),
         dot_balls = if_else(total_runs== 0,1, 0)) %>%
  group_by(season, match_id, batting_team,
           bowling_team,  inning, over) %>%
  summarise(wicket = sum(wicket),
            dot_balls = sum(dot_balls),
            runs_per_over = sum(total_runs),
            run_rate = round(sum(total_runs)/length(total_runs))) %>% ungroup()


cricket %>%
  left_join(cricket_winner, by= c("match_id"))%>%
  select(season, match_id, batting_team,
         bowling_team,
         inning,
         over,
         wicket,
         dot_balls,
         runs_per_over,
         run_rate,
         winner_team)



save(cricket, file = "data/cricket.rda", compress = "gzip", version = 2)



