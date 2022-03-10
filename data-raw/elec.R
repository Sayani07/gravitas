library(readr)
library(dplyr)
library(tsibble)
# devtools::install_github("jimhester/archive")
library(archive)

# Raw data obtained from Electricity Use Interval Reading of Smart-Grid Smart-City Customer Trial Data available in
# https://data.gov.au/dataset/ds-dga-4e21dea3-9b87-4610-94c7-15a8a77907ef/distribution/dist-dga-b71eb954-196a-4901-82fd-69b17f88521e/details?q=smart%20meter
# file name:CD_INTERVAL_READING_ALL_NO_QUOTES.csv

if (!file.exists("data-raw/smart-metre.7z")) {
  download.file(
    "http://datagovau.s3-ap-southeast-2.amazonaws.com/CDINTERVALREADINGALLNOQUOTES.csv.7z",
    "data-raw/smart-metre.7z",
    mode = "wb"
  )
}

smart_meter_data_raw <- archive_read("data-raw/smart-metre.7z") %>%
  read_csv(n_max = 3e6)

set.seed(12345)
sm_cust <- smart_meter_data_raw %>%
  dplyr::distinct(CUSTOMER_ID) %>%
  pull(CUSTOMER_ID) %>%
  sample(size = 50)

sm_cust <- smart_meter_data_raw %>%
  filter(CUSTOMER_ID %in% sm_cust) %>%
  rename_all(tolower) %>%
  arrange(customer_id, reading_datetime) %>%
  group_by(customer_id) %>%
  dplyr::mutate(
    reading_datetime = case_when(
      duplicated(reading_datetime) ~ reading_datetime + lubridate::hours(1),
      TRUE ~ reading_datetime
    )
  ) %>%
  ungroup() %>%
  dplyr::mutate(customer_id = as.character(customer_id)) %>%
  as_tsibble(index = reading_datetime, key = customer_id) %>%
  select(-calendar_key) %>%
  select(
    customer_id,
    reading_datetime,
    general_supply_kwh
  )


set.seed(12345)
sm10 <- sm_cust %>%
  dplyr::distinct(customer_id) %>%
  dplyr::slice_sample(n = 7)

smart_meter10 <- sm_cust %>% filter(customer_id %in% c(sm10$customer_id, "10006704", "10017936", "10006486"))


usethis::use_data(smart_meter10, overwrite = TRUE, compress = "xz")
#
# save(sm_cust10, file = "data/sm_cust50.rds", compress = "xz")
