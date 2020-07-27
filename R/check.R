smart_meter10 %>%
dplyr::filter(customer_id %in% c("10017936")) %>%
prob_plot(
gran1 = "week_month", gran2 = "wknd_wday",
response = "general_supply_kwh", plot_type = "quantile",
quantile_prob = c(0.1, 0.25, 0.5, 0.75, 0.9),
symmetric = TRUE,
outlier.colour = "red",
outlier.shape = 2, palette = "Dark2"
)


data_mutate_obj %>% ggplot(aes(x = hour_day, group = day_week)) + geom_ribbon(aes(ymin = `25%`, ymax = `75%`), fill = "grey70") + geom_line(aes(y = `50%`)) + facet_wrap(~day_week)


data_mutate_obj %>% tidyr::pivot_longer(cols =  c(`25%`,`50%`,`75%`), names_to = "quantiles") %>% ggplot() + geom_line(aes(x = hour_day, y = value, fill= quantiles, group= day_week)) +
  facet_wrap(~day_week)



library(dplyr)
library(ggplot2)

tibble(x = 1:10) %>%
  group_by_all() %>%
  do(tibble(y = rnorm(100, .$x))) %>%
  median_qi(.width = c(.5, .8, .95)) %>%
  ggplot(aes(x = x, y = y, ymin = .lower, ymax = .upper)) +
  # automatically uses aes(fill = fct_rev(ordered(.width)))
  geom_lineribbon() +
  scale_fill_brewer()


smart_meter10 %>%
  dplyr::filter(customer_id %in% c("10017936")) %>%
  create_gran("hour_day") %>%
  create_gran("day_week") %>%
  as_tibble() %>%
  select(hour_day,
         day_week,
         general_supply_kwh) %>%
  group_by(hour_day, day_week) %>%
  median_qi(.width = c(.25, .50, .75)) %>%
  ggplot(aes(x = hour_day,
             y = general_supply_kwh,
             ymin = .lower,
             ymax = .upper,
             group = day_week)) +
  geom_lineribbon() +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~day_week) +
  scale_x_discrete(breaks = seq(0, 23, 5))+
  labs(fill = "percentile")



data_mutate_obj %>%


smart_meter10 %>%
dplyr::filter(customer_id %in% c("10017936")) %>%
prob_plot(
  gran1 = "day_week", gran2 = "hour_day",
  response = "general_supply_kwh",
  plot_type = "quantile",
  quantile_prob = c(0.25, 0.5, 0.75),
  symmetric = TRUE,
  outlier.colour = "red",
  outlier.shape = 2, palette = "Dark2")
