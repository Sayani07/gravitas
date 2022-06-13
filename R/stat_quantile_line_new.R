data <- smart_meter10 %>% filter(customer_id=="10017936")
x <- "hour_day"
y <- "general_supply_kwh"
f <- "day_week"

result <- data %>%
  create_gran(gran1 = x)%>%
  create_gran(gran1 = f)%>%
  tibble::as_tibble() %>%
  select(!!rlang::sym(x),!!rlang::sym(f), !!rlang::sym(y)) %>%
  group_split(!!rlang::sym(x), !!rlang::sym(f)) %>%
  bind_rows()



calc_quantile_and_color <- function(x, quantile_prob = c(0.25, 0.5, 0.75)) {
  tibble(y = stats::quantile(x, probs = quantile_prob)) %>%
    mutate(color = quantile_prob)
}


calc_median_and_color <- function(x, threshold = 0.5, probs) {
  tibble(y = quantile(x, probs)) %>%
    mutate(fill = ifelse(y < threshold, "pink", "grey35"))
}


p <- ggplot(result,
            aes(x = hour_day, y = general_supply_kwh)) +
  #facet_wrap(~day_week) +
  stat_summary(geom = "line",
               fun = calc_median_and_color,
               fun.args = list(probs = c(0.25, 0.5, 0.75)))


quantile(p$general_supply_kwh, na.rm  = TRUE)
layer_data(p)

