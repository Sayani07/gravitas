library(ggplot2)
library(gravitas)
library(purrr)
library(distributional)
library(magrittr)
library(tidyr)
harmonies <- tibble::tibble(facet_variable = c("A", "B"),x_variable  = c("B","A"), facet_levels = c(2, 3),x_levels = c(3, 2))
data <- sim_distharmony1(.data = harmonies[1,], sim_dist =  c(rep(dist_normal(mu = 2, sigma = 5),3), rep(dist_normal(mu = 5, sigma = 10), 3)))
data %>% select(-dist) %>% unnest(sim_dist) %>%
ggplot(aes(x = Var2, y = sim_dist)) +
facet_wrap(~Var1) + geom_boxplot()
data %>% select(-dist) %>% unnest(sim_dist) %>%
ggplot(aes(x = Var1, y = sim_dist)) +
facet_wrap(~Var2) + geom_boxplot()
 dat1 <- data %>% select(-dist) %>%
 unnest(sim_dist) %>%
 pivot_wider(names_from = Var1, values_from  = sim_dist)
 dat2 <- data %>% select(-dist) %>%
  unnest(sim_dist) %>%
  pivot_wider(names_from = Var2, values_from  = sim_dist)
  step1_data <- list(dat1, dat2)
  response = "general_supply_kwh"
  global_harmony_iter1 <-  smart_meter10 %>%
  global_threshold(harmony_tbl = harmonies,
  response, step1_data = step1_data,
   response, hierarchy_tbl = hierarchy_model)
