# Iteration 1: all distributions are equal

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

step1_data <-list(
  data_1 %>%
    arrange(Var2, Var1) %>%
    slice(rep(1:n(), each = ntimes)) %>%
    mutate(sim_dist = unlist((1:length(lev_B)) %>%
                               purrr::map(function(i){
                                 dist_normal(mu = i, sigma = 5*i) %>% generate(ntimes*nA)
                               })
    )) %>%
    pivot_wider(names_from = Var1, values_from = sim_dist,  values_fn = list(sim_dist = list)),


  data_2 %>%
    arrange(Var2, Var1) %>%
    slice(rep(1:n(), each = ntimes)) %>%
    mutate(sim_dist = unlist((1:length(lev_B)) %>%
                               purrr::map(function(i){
                                 dist_normal(mu = i, sigma = 5*i) %>% generate(ntimes*nC)
                               })
    ))%>%
    pivot_wider(names_from = Var1, values_from = sim_dist,  values_fn = list(sim_dist = list)),

  data_3 %>%
    arrange(Var2, Var1) %>%
    slice(rep(1:n(), each = ntimes)) %>%
    mutate(sim_dist = unlist((1:length(lev_C)) %>%
                               purrr::map(function(i){
                                 dist_normal(mu = i, sigma = 5*i) %>% generate(ntimes*nD)
                               })
    )) %>%
    pivot_wider(names_from = Var1, values_from = sim_dist,  values_fn = list(sim_dist = list)),


  data_4 %>%
    arrange(Var2, Var1) %>%
    slice(rep(1:n(), each = ntimes)) %>%
    mutate(sim_dist = unlist((1:length(lev_E)) %>%
                               purrr::map(function(i){
                                 dist_normal(mu = i, sigma = 5*i) %>% generate(ntimes*nD)
                               })
    )) %>%
    pivot_wider(names_from = Var1, values_from = sim_dist,  values_fn = list(sim_dist = list))

)

# data doesn't matter after step1_data has been fed in
global_harmony_iter3 <-  smart_meter10 %>% global_threshold(harmony_tbl = harmonies, response, step1_data = step1_data, hierarchy_tbl = hierarchy_model)


sim_data3 <- step1_data %>% enframe() %>% unnest(value) %>% pivot_longer(c(-name, -Var2), names_to = "Var1", values_to = "sim_data") %>% unnest(sim_data) %>% group_by(name) %>% mutate(ind = row_number()) %>% as_tsibble(index = ind, key = name)

sim_data3%>%
  filter(name==1) %>%
  ggplot(aes(x = Var2, y = sim_data)) + geom_boxplot() + facet_wrap(~Var1)

