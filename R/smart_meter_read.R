library(readr)
library(tidyverse)
library(lubridate)
library(tsibble)

smart_meter_data_raw <- read_csv("data/CD_INTERVAL_READING_ALL_NO_QUOTES.csv", n_max = 1e6)

# This data has duplicated rows for many households during October for both 2012 and 2013


smart_meter_data <-  smart_meter_data_raw %>%
  rename_all(tolower) %>%
  arrange(customer_id, reading_datetime) %>%
  group_by(customer_id) %>%
  mutate(reading_datetime = case_when(
    duplicated(reading_datetime) ~ reading_datetime + lubridate::hours(1),
    TRUE ~ reading_datetime
  ))

smart_meter_data$customer_id <- as.character(smart_meter_data$customer_id)


smart_meter<-  smart_meter_data %>%
  as_tsibble(index = reading_datetime, key = customer_id) %>%
  ungroup() %>%
  select(-calendar_key)

save(smart_meter, file = "data/smart_meter.RData")


