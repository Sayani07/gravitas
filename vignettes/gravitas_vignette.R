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


## ---- echo=FALSE, out.width="100%"---------------------------------------
# smart_p <- smart_meter10 %>%
#            ggplot() + 
#            geom_line(aes(x =reading_datetime,
#                          y = general_supply_kwh, 
#                          color = customer_id))  + 
#   theme(legend.position = "None")
# 
# smart_anim <-  smart_p + gganimate::transition_states(customer_id)+ labs(title = "{closest_state}")
# 
# gganimate::animate(smart_anim, fps = 10, width = 1000, height = 600)
# 
# 
# anim_save("images/smart_allcust.gif")

knitr::include_graphics("man/figures/smart_allcust.gif")

## ---- echo=FALSE, out.width="100%"---------------------------------------
# smart_p <- smart_meter10 %>%
#            ggplot() + 
#            geom_line(aes(x =reading_datetime,
#                          y = general_supply_kwh, 
#                          color = customer_id))  + 
#   theme(legend.position = "None")
# 
# smart_anim <-  smart_p + gganimate::transition_states(customer_id)+ labs(title = "{closest_state}")
# 
# gganimate::animate(smart_anim, fps = 10, width = 1000, height = 600)
# 
# 
# anim_save("images/smart_allcust.gif")

knitr::include_graphics("man/figures/smart_allcust_period.gif")

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

smart_meter10 %>% 
  search_gran(
  highest_unit = "month",
  filter_out = c("hhour", "fortnight")
)

## ----create_gran, echo=TRUE----------------------------------------------
library(lvplot)
library(ggplot2)
library(dplyr)
library(tibble)

smart_meter10 %>%
  filter(customer_id %in% c(10006704, 10017936)) %>% 
  create_gran("day_fortnight") %>%
  as_tibble() %>% 
  group_by(customer_id, day_fortnight) %>% 
  summarise(median_demand = median(general_supply_kwh)) %>% 
  ggplot(aes(x=day_fortnight, y =median_demand,
             color = customer_id, group = customer_id)) + 
  geom_line()

## ------------------------------------------------------------------------
smart_meter10 %>%
  filter(customer_id %in% c(10006704, 10017936)) %>% 
  create_gran("day_fortnight") %>%
  ggplot2::ggplot(aes(
    x = as.factor(day_fortnight),
    y = general_supply_kwh)) +
  scale_y_sqrt() +
  xlab("day_fortnight") +
  geom_lv(
    outlier.colour = "red",
    aes(fill = ..LV..),
    k = 5) + facet_wrap(~customer_id)


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
  filter_out = c("hhour", "fortnight")
)

## ----hd_dm, echo=TRUE, fig.cap="Quantile plot across hour-day as facets and day-month across x-axis"----
smart_meter10 %>%
  prob_plot("hour_day",
    "day_month",
    response = "general_supply_kwh",
    plot_type = "quantile",
    quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
    symmetric = TRUE
  ) +
  scale_y_sqrt()

## ----dm_hd, echo=TRUE----------------------------------------------------
smart_meter10 %>%
  prob_plot("day_month",
    "hour_day",
    response = "general_supply_kwh",
    plot_type = "quantile",
    quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
    symmetric = TRUE
  )

## ----hd_dw, echo=TRUE----------------------------------------------------
smart_meter10 %>% prob_plot("wknd_wday",
  "week_month",
  response = "general_supply_kwh",
  plot_type = "violin"
) + scale_y_sqrt()

## ----granobs, echo=TRUE--------------------------------------------------
smart_meter10 %>% gran_obs(
  "week_month",
  "wknd_wday"
)

## ----granadvice----------------------------------------------------------
smart_meter10 %>% gran_advice(
  "week_month",
  "wknd_wday"
)

