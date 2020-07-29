(1: length(step1_data)) %>%
  purrr::map(function(rowi){
    step_datai <- step1_data %>%
      magrittr::extract2(rowi)
    z <- step_datai %>% pivot_longer(-1, names_to = "gran1", values_to = "value")%>% tidyr::unnest(value)
    names(z)[1] = "gran2"
    return(z)
  })

 library(ggplot2)
 library(gravitas)
 library(purrr)
 library(distributional)
 library(magrittr)
 library(tidyr)
 harmonies <- tibble::tibble(facet_variable = c("A", "B"),x_variable  = c("B","A"), facet_levels = c(2, 3),x_levels = c(3, 2))
.data = harmonies[1,]
data <- sim_distharmony1(.data, sim_dist =  c(rep(distributional::dist_normal(mu = 2, sigma = 5),3), rep(distributional::dist_normal(mu = 5, sigma = 10), 3)))

data_l = bind_cols(pairn = 1L, data) %>% select(-dist) %>% unnest(sim_dist)
data_m = bind_cols(pairn = 2L, data) %>% select(-dist) %>% unnest(sim_dist)
data_mlist =  list(data_l, data_m)

sim_harmony <- map(data_mlist, ~ (.x %>% select(-1)))  %>%
  rank_harmony(harmony_tbl = harmonies,
             response = "sim_dist",
             prob = seq(0.01, 0.99, 0.01),
             dist_distribution = "normal",
             hierarchy_tbl = NULL,
             dist_ordered = TRUE,
             alpha = 0.05,
             create_gran_data = FALSE)

global_harmony <- map(data_mlist, ~ (.x %>% select(-1))) %>%
  global_threshold(harmony_tbl = harmonies,
               response = "sim_dist",
               dist_distribution = "normal",
               dist_ordered = TRUE,
               create_gran_data = FALSE)





