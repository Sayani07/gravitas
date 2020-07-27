library(tidyverse)
library(distributional)
library(gravitas)
harmonies <- tibble::tibble(facet_variable = c("A", "B"),
                            x_variable  = c("B","A"),
                            facet_levels = c(2, 3),
                            x_levels = c(3, 2))




nA <-2
nB <- 3

lev_A <- paste0("A", 1:nA)
lev_B <- paste0("B", 1:nB)

data_1 <- expand.grid(lev_A, lev_B)
data_2 <- expand.grid(lev_B, lev_A)

ntimes <- 500


step1_data <- list(
  bind_cols(data_1 %>% slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nA*nB)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list)),

  bind_cols(data_2 %>% slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nB*nA)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list)))


# simulate same distribution for all categories of a harmony pair

sim_distharmony1 <- function(.data,
                                ntimes = 500,
                                sim_dist = dist_normal(mu = 2, sigma = 5)){

  nfacet <- .data$facet_levels
  nx <- .data$x_levels

  lev_facet <- paste0(.data$facet_variable, 1:nfacet)
  lev_x <- paste0(.data$x_variable, 1:nx)

  data_1 <- expand.grid(lev_facet, lev_x)

  bind_cols(data_1 %>%
    slice(rep(1:n(), each = ntimes)),
  sim_dist %>%
    generate(ntimes*nfacet*nx)) %>%
  as_tibble() %>%
  pivot_wider(names_from = Var1,
              values_from = ...3,
              values_fn = list(...3 = list))
}


# simulate different distribution for facets and same for x

sim_distharmony1 <- function(.data,
                             ntimes = 500,
                             sim_dist = dist_normal(mu = 2, sigma = 5)){

  nfacet <- .data$facet_levels
  nx <- .data$x_levels

  len_sim_dist <- sim_dist %>% lengths %>% length()

  # if only one distribution is given, repeat it for all categories

  if(len_sim_dist==1)
  {
    sim_dist = rep(sim_dist, nfacet*nx)
  }

  lev_facet <- paste0(.data$facet_variable, 1:nfacet)
  lev_x <- paste0(.data$x_variable, 1:nx)

  data_1 <- expand.grid(lev_facet, lev_x) %>%
    mutate(dist = sim_dist)
#sim_dist <- dist_normal(mu = 1:6, sigma = 3)

data_1 %>% group_by(Var1, Var2)  %>%
  mutate(sim_dist = generate(dist, ntimes))
}

create_sim_pair <- function(data1,
                            data2,
                            ntimes,
                            sim_dist = dist_normal(mu = 2, sigma = 5))
{
  nA <-data1 %>% distinct(Var1) %>% nrow()
  nB <-data1 %>% distinct(Var2) %>% nrow()

  list(
    bind_cols(data1 %>% slice(rep(1:n(), each = ntimes)), sim_dist %>%
                generate(ntimes*nA*nB)) %>%
      as_tibble() %>%
      pivot_wider(names_from = Var1,
                  values_from = ...3,
                  values_fn = list(...3 = list)),

    bind_cols(data2 %>% slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nB*nA)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                      values_from = ...3,
                                                                                                                                                      values_fn = list(...3 = list)))

}



response = "general_supply_kwh"
global_harmony_iter1 <-  smart_meter10 %>% global_threshold(harmony_tbl = harmonies, response, step1_data = step1_data, response, hierarchy_tbl = hierarchy_model)



sim_data1 <- step1_data %>% enframe() %>% unnest(value) %>% pivot_longer(c(-name, -Var2), names_to = "Var1", values_to = "sim_data") %>% unnest(sim_data) %>% group_by(name) %>% mutate(ind = row_number()) %>% as_tsibble(index = ind, key = name)

g1 <- sim_data1 %>%
  filter(name==1) %>%
  ggplot(aes(x = Var2, y = sim_data)) + geom_boxplot() + facet_wrap(~Var1)

g2 <- sim_data1 %>%
  filter(name==2) %>%
  ggplot(aes(x = Var2, y = sim_data)) + geom_boxplot() + facet_wrap(~Var1)

ggarrange(g1, g2, nrow = 2)
