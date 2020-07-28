library(tsibbledata)
library(ggplot2)
library(tsibble)
library(lvplot)
library(dplyr)
library(gravitas)
library(purrr)
library(magrittr)
library(distributional)
sm <- smart_meter10 %>%
filter(customer_id %in% c("10017936"))

#sim_data <- sm %>% mutate(sim_var = rnorm(nrow(sm), 0, 1))
sim_data <- sm %>% mutate(sim_var = rexp(nrow(sm), 1))

#harmonies <- sim_data %>%
#harmony(ugran = "month",
        #filter_in = "wknd_wday",
        #filter_out = c("hhour", "fortnight"))
# number of levels of gran1 and gran2

lengran1 = 5
lengran2 = 3

gran1 = seq(1, lengran1, by = 1)
gran2 = seq(1, lengran2, by = 1)

response  = "sim_var"
harmony_tbl =  harmonies[c(1,3,2,6),]

smart_harmony <- sim_data %>% rank_harmony(harmony_tbl = harmony_tbl,response, dist_ordered = TRUE)

global_harmony <-  sim_data %>% global_threshold(harmony_tbl = harmony_tbl, response)

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

  step1_data <- list(
  dplyr::bind_cols(data_1 %>% dplyr::slice(rep(1:dplyr::n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nA*nB)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list)),

  dplyr::bind_cols(data_2 %>% dplyr::slice(rep(1:dplyr::n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nC*nB)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list)),


  dplyr::bind_cols(data_3 %>% dplyr::slice(rep(1:dplyr::n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nD*nC)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list)),


  dplyr::bind_cols(data_4 %>% dplyr::slice(rep(1:dplyr::n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes*nD*nE)) %>% as_tibble() %>% pivot_wider(names_from = Var1,
                                                                                                                                                    values_from = ...3,
                                                                                                                                                    values_fn = list(...3 = list))
  )

iter1 <- (1:nrow(data_lev)) %>%
      purrr::map_df(function(i){
        dplyr::bind_cols(data_lev[i,] %>% dplyr::slice(rep(1:n(), each = ntimes)), dist_normal(mu = 2, sigma = 5) %>% generate(ntimes))    })

iter1 <- iter1 %>%
  mutate(index = row_number()) %>%
  tsibble::as_tsibble(index = index) %>%
  rename("sim_dist" = "...3")

iter1_result <- iter1 %>% global_threshold(harmony_tbl = harmonies, response = "sim_dist")

global_harmony <-  iter1 %>% global_threshold(harmony_tbl = harmonies, response, step1_data = step1_data, hierarchy_tbl = hierarchy_model)

