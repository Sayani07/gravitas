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

## ----hierarchy, echo=FALSE-----------------------------------------------
hierarchy_model <- tibble::tibble(units = c("index", "ball", "over", "inning", "match"), 
                                  convert_fct = c(1, 6, 20, 2, 1))
knitr::kable(hierarchy_model, caption = " A hierarchy table for T20 cricket")

## ------------------------------------------------------------------------
cricket_tsibble <- cricket %>%
  ungroup() %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index)

## ------------------------------------------------------------------------
cricket_tsibble %>%
  filter(batting_team == "Mumbai Indians") %>% 
    prob_plot("inning", "over",
             hierarchy_model,
             response = "runs_per_over",
             plot_type = "lv")

cricket_tsibble %>%
  filter(batting_team == "Delhi Daredevils") %>% 
    prob_plot("inning", "over",
             hierarchy_model,
             response = "runs_per_over",
             plot_type = "lv")

