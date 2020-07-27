# Iteration 1: all distributions are equal
library(distributional)
library(tidyr)
hierarchy_model <- tibble::tibble(
  units = c("A", "B", "C", "D"),
  convert_fct = c(3, 5, 7, 24)
)

nA <-3
nB <- 5
nC <- 7
nD <- 24
nE <- 31

harmonies <- tibble::tibble(facet_variable = c("A", "C","D", "D"),
                            x_variable  = c("B","B", "C", "E"),
                            facet_levels = c(nA, nC, nD, nD),
                            x_levels = c(nB, nB, nC, nE))

lev_A <- paste0("A", 1:nA)
lev_B <- paste0("B", 1:nB)
lev_C <- paste0("C", 1:nC)
lev_D <- paste0("D", 1:nD)
lev_E <- paste0("E", 1:nE)


data_1 <- expand.grid(lev_A, lev_B)
data_2 <- expand.grid(lev_C, lev_B)
data_3 <- expand.grid(lev_D, lev_C)
data_4 <- expand.grid(lev_D, lev_E)
#data_lev <- dplyr::bind_rows(data_1, data_2, data_3, data_4)
ntimes <- 500

step1_data <- list(
  bind_cols(data_1 %>% slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nA*nB)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list)),

  bind_cols(data_2 %>% slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nC*nB)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list)),


  bind_cols(data_3 %>% slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nD*nC)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list)),


  bind_cols(data_4 %>% slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nD*nE)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list))
)
#
# iter1 <- (1:nrow(data_lev)) %>%
#   purrr::map_df(function(i){
#     bind_cols(data_lev[i,] %>% slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes))    })
#
# iter1 <- iter1 %>%
#   mutate(index = row_number()) %>%
#   tsibble::as_tsibble(index = index) %>%
#   rename("sim_dist" = "...3")

global_harmony_iter1 <-  smart_meter10 %>% global_threshold(harmony_tbl = harmonies, response, step1_data = step1_data, hierarchy_tbl = hierarchy_model)

sim_data1 <- step1_data %>% enframe() %>% unnest(value) %>% pivot_longer(c(-name, -Var2), names_to = "Var1", values_to = "sim_data") %>% unnest(sim_data) %>% group_by(name) %>% mutate(ind = row_number()) %>% as_tsibble(index = ind, key = name)

sim_data1 %>%
  filter(name==1) %>%
  ggplot(aes(x = Var2, y = sim_data)) + geom_boxplot() + facet_wrap(~Var1)
