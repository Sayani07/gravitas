library(tidyverse)
library(tsibble)
library(gravitas)
library(readr)

# smart_meter_MRS
# smart-meter13.rds is obtained from tsibble package

  smart_meter13 <- read_rds("inst/extdata/smart-meter13.rds")
#   # save(smart_meter13, file = "data/smart-meter13.RData", compress = "xz")
#   #
#
# smart_meter13 <- read_rds("data/smart-meter13.R")
 smart_meter_13 <-smart_meter13 %>% dplyr::select(customer_id, reading_datetime,   general_supply_kwh)

 smart_meter_13_ts <- smart_meter_13 %>% as_tsibble(index = reading_datetime, key = customer_id)


 sm_cust1 <-  smart_meter_13_ts %>% dplyr::filter(customer_id == 8144679)

 save(sm_cust1, file = "data/sm_cust1.RData", compress = "xz")

#usethis::use_data(sm_cust1, overwrite = TRUE)




## Run in Di's computer for entire one

#smart_meter_data <- read_csv("data/CD_INTERVAL_READING_ALL_NO_QUOTES.csv")

# smart_meter_customers <- smart_meter_data %>% distinct(CUSTOMER_ID)
# smart_customer_50 <- smart_meter_customers %>% head(n = 50)
#
# smart_meter_50 <- smart_meter_data %>%
#                   filter(CUSTOMER_ID %in% smart_customer_50$CUSTOMER_ID) %>%
#                   rename_all(tolower) %>%
#                   mutate(reading_datetime = case_when(
#                     duplicated(reading_datetime) ~ reading_datetime + hours(1),
#                     TRUE ~ reading_datetime
#                     )) %>%
#   arrange(customer_id, reading_datetime)
# write_rds(smart_meter_50, "data/smart_meter_50_tbl.rds", compress = "xz")
#
# smart_meter_50 <- tsibble(smart_meter_50, index = reading_datetime, key = id(customer_id))

