    # smart_meter_data_raw <- read_csv("data/CD_INTERVAL_READING_ALL_NO_QUOTES.csv", n_max = 3e6)
    # set.seed(12345)
    # sm_cust <- smart_meter_data_raw %>% distinct(CUSTOMER_ID) %>% .$CUSTOMER_ID %>% sample(size= 50)
    # smart_meter_data_raw <- smart_meter_data_raw %>% dplyr::filter(CUSTOMER_ID %in% sm_cust)
    # write_rds(smart_meter_data_raw, "data/smart_meter_data_raw.rds", compress = "xz")


library(tidyverse)
library(tsibble)
library(gravitas)
library(readr)

    smart_meter_data_raw <- read_rds("data/smart_meter_data_raw.rds")

smart_meter_data <- smart_meter_data_raw %>%
  dplyr::rename_all(tolower) %>%
  dplyr::arrange(customer_id, reading_datetime) %>%
  dplyr::group_by(customer_id) %>%
  dplyr::mutate(reading_datetime = case_when(
    duplicated(reading_datetime) ~ reading_datetime + lubridate::hours(1),
    TRUE ~ reading_datetime
  ))

smart_meter_data$customer_id <- as.character(smart_meter_data$customer_id)

sm_cust50 <- smart_meter_data %>%
  as_tsibble(index = reading_datetime, key = customer_id) %>%
  ungroup() %>%
  dplyr::select(-calendar_key)

# selecting just 10
set.seed(1200)
sm10 <- sm_cust50 %>%
  distinct(customer_id) %>%
  dplyr:: slice_sample(n = 8)

smart_meter10 <- sm_cust50 %>% filter(customer_id %in% c( sm10$customer_id, "10006704", "10017936"))

smart_meter10 <- smart_meter10 %>%
                 as_tsibble() %>%
                 select(customer_id,p;
                        reading_datetime,
                        general_supply_kwh)


save(smart_meter10, file = "data/smart_meter10.RData", compress = "xz")
