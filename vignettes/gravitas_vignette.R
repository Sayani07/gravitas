## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width  = 8,
  fig.align  = "center",
  cache      = FALSE
)
library(gravitas)
library(dplyr)
library(ggplot2)
library(tsibble)

## ----data_load, echo=FALSE-----------------------------------------------
load("../data/smart13_cust2.rda")

## ----data_store, echo=TRUE-----------------------------------------------
smart_meter_nsw <- smart_meter_13_cust2 %>% as_tsibble()

