library(readr)
library(tsibble)
library(dplyr)
# cricket_data <-  read_csv("data/cricket.csv")

# with key tsibble
cricket <- cricket_data %>%
  group_by(batting_team) %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index, key = batting_team)

save(cricket, file = "data/cricket.RData")


hierarchy <- tibble(units = c("ball", "over", "quarter", "semester", "match"), convert_fct = c(NA, 5, 2, 2, 1))
hierarchy
