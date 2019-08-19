library(shiny)
library(plotly)
library(gravitas)
library(dplyr)

so_many_problems <- function(){
  message("msg")
  message("msg2")
  warning("warn")
  warning("warn2")
  warning("warn3")
  if(runif(1) > 0.5){
    stop("error")
  }
  "yay"
}

capture_all_problems <- function(expr){
  msg <- list()
  wrn <- list()
  err <- NULL

  withCallingHandlers(
    result <- tryCatch(
      expr,
      error = function(e){
        err <<- e$message
        NULL
      }
    ),
    message = function(m){
      msg[[length(msg) + 1]] <<- m$message
      invokeRestart("muffleMessage")
    },
    warning = function(w){
      wrn[[length(wrn) + 1]] <<- w$message
      ops <- options(warn = -1)
      on.exit(options(ops))
      invokeRestart("muffleWarning")
    }
  )

  list(
    result = result,
    message = msg,
    warning = wrn,
    error = err
  )
}

capture_all_problems(so_many_problems())



modes <- getAceModes()
themes <- getAceThemes()

init <-
"library(shiny)
library(shinyAce)
library(gravitas)

granplot(.data, gran1 = NULL, gran2 = NULL, response = NULL, plot_type = NULL, quantile_prob = seq(0.1,0.9,0.1),  facet_h = 31, ...)"

