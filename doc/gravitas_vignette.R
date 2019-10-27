## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width = 8,
  fig.align = "center",
  cache = FALSE
)
library(gravitas)
library(dplyr)
library(ggplot2)
library(tsibble)


## ----data_look-----------------------------------------------------------
smart_meter10 %>% select(customer_id,
                         reading_datetime,
                         general_supply_kwh,
                         everything()
)

## ----search_gran---------------------------------------------------------
library(tsibble)
interval(smart_meter10)
smart_meter10 %>% search_gran()

## ----search_gran_limit---------------------------------------------------

smart_meter10 %>% 
  search_gran(highest_unit = "month")

## ----search_gran_limit2--------------------------------------------------

smart_meter10 %>% search_gran(highest_unit = "month",
                              filter_out = c("hhour", "fortnight")
)

## ------------------------------------------------------------------------
library(lvplot)
library(ggplot2)
library(dplyr)
library(tibble)
smart_meter10 %>%
  filter(customer_id %in% c(10006704, 10017936)) %>% 
  create_gran("day_fortnight") %>%
  ggplot2::ggplot(aes(
    x = as.factor(day_fortnight),
    y = general_supply_kwh)) +
  xlab("day_fortnight") +
  geom_lv(
    outlier.colour = "red",
    aes(fill = ..LV..),
    k = 5) +
  facet_wrap(~customer_id) + 
  scale_fill_lv() +
  theme_bw() +
  scale_y_sqrt() 


## ----is_harmony----------------------------------------------------------
smart_meter10 %>% 
  is_harmony(gran1 = "hour_day", 
             gran2 = "day_week")

smart_meter10 %>%
  is_harmony(gran1 = "hour_day", 
             gran2 = "day_week", 
             facet_h = 14)

smart_meter10 %>% 
  is_harmony(gran1 = "day_month",
             gran2 = "week_month")

## ----harmony, echo=TRUE--------------------------------------------------
smart_meter10 %>% harmony(
  ugran = "month",
  filter_out = c("hhour")
)

## ----granadvice----------------------------------------------------------
smart_meter10 %>% gran_advice(
  "week_month",
  "hour_day"
)

## ----granobs, echo=TRUE--------------------------------------------------
smart_meter10 %>% gran_obs(
  "week_month",
  "wknd_wday"
)

## ----hd_dw, echo=TRUE----------------------------------------------------
cust1 <- smart_meter10 %>% 
  filter(customer_id %in% c(10006704)) %>% 
  prob_plot("wknd_wday",
                            "hour_day",
                            response = "general_supply_kwh",
                            plot_type = "boxplot") +
  scale_y_sqrt()

## ----hd_dw2, echo=TRUE---------------------------------------------------
cust2 <- smart_meter10 %>% 
  filter(customer_id %in% c(10017936))%>% 
  prob_plot("wknd_wday",
                            "hour_day",
                            response = "general_supply_kwh",
                            plot_type = "boxplot") +
  scale_y_sqrt()

## ----cust, echo=FALSE----------------------------------------------------
cust1
cust2

