library(readr)
library(tsibble)
library(dplyr)
cricket_data <-  read_csv("data/cricket.csv")

# with key tsibble
cricket <- cricket_data %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index, key = batting_team)

save(cricket, file = "data/cricket.rds")
