## ----setup, include = FALSE---------------------------------------------------
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
library(lvplot)

## ----readdata-----------------------------------------------------------------
library(gravitas)
library(tibble)
glimpse(cricket)

## ----hierarchy2---------------------------------------------------------------

hierarchy_model <- tibble::tibble(
  units = c("index", "over", "inning", "match"),
  convert_fct = c(1, 20, 2, 1))

knitr::kable(hierarchy_model, format = "markdown")


## ----crickread----------------------------------------------------------------
library(tsibble)
cricket_tsibble <- cricket %>%
  mutate(data_index = row_number()) %>%
  as_tsibble(index = data_index)

