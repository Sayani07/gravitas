## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  echo       = TRUE,   # code
  include    = TRUE,   # plots
  results    = "hide", # text: "hide", "show"
  eval       = TRUE,   # chunk
  message    = FALSE,
  warning    = FALSE,
  error      = FALSE,
  collapse   = TRUE,
  comment    = "#>",
  fig.height = 4,
  fig.width  = 6,
  fig.align  = "center",
  cache      = FALSE
)
library(gravitas)
library(dplyr)
library(ggplot2)
library(tsibble)

