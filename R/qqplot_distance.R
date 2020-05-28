#' Plotting QQ-plot of distances between probability distributions
#'
#' Plotting empirical and theoretical distribution of distances to understand best fit
#'
#' @param .data a tsibble.
#' @param response response variable.
#' @param gran1 the granularity which is to be placed across facets. Can be column names if required granularity already exists in the tsibble. For example, a column with public holidays which needs to be treated as granularity, can be included here.
#' @param gran2 the granularity to be placed across x-axis. Can be column names if required granularity already exists in the tsibble.
#' @param prob numeric vector of probabilities with values in [0,1].
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units
#' @return  A tibble of harmonies and their levels ranked ion descending order of average maximum pairwise distance of the harmony pairs.
#
#' @examples
#' library(tsibbledata)
#' library(ggplot2)
#' library(tsibble)
#' library(lvplot)
#' library(dplyr)
#' library(gravitas)
#' library(purrr)
#' library(magrittr)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c(10017936))
#' .data = sm
#' gran1 = "wknd_wday"
#' gran2 = "hour_day"
#' response  = "general_supply_kwh"
#' .data %>% qqplot_distance(gran1 = "wknd_wday", gran2 = "hour_day", response = "general_supply_kwh")
#' .data %>% qqplot_distance(gran1 = "day_week", gran2 = "hour_day", response = "general_supply_kwh")
#' @export qqplot_distance
qqplot_distance <- function(.data = NULL,
                            gran1 = NULL,
                            gran2 = NULL,
                            response = NULL,
                            prob = seq(0.01, 0.99, 0.01),
                            hierarchy_tbl = NULL)
{
  new_distvector <- hist_distance(.data,
                                  gran1,
                                  gran2,
                                  response,
                                  prob = seq(0.01, 0.99, 0.01),
                                  hierarchy_tbl)
  graphics::par(mfrow = c(2, 2))
  #
  #    if(dist_distribution=="normal")
  #    {
  emp_var <- new_distvector$freqx
  emp_shape <- (stats::sd(emp_var)/ mean(emp_var))^(-1.086)
  emp_scale <- mean(emp_var)/gamma(1+1/emp_shape)

  y = stats::qnorm(stats::ppoints(length(emp_var)))
  # qqplot(y, new_distvector$freqx,
  #             xlab = "theoretical",
  #             ylab = "empirical",
  #             main = "normal")

  EnvStats::qqPlot(new_distvector$freqx, distribution = "norm", estimate.params = T, add.line = TRUE,  ylab = "empirical",main = "Normal")


  #
  # if(dist_distribution=="weibull")
  # {
  emp_var <- new_distvector$freqx
  emp_shape <- (stats::sd(emp_var)/ mean(emp_var))^(-1.086)
  emp_scale <- mean(emp_var)/gamma(1+1/emp_shape)

  y = stats::qweibull(stats::ppoints(length(emp_var)), shape = emp_shape, scale = emp_scale)
  # qqplot(y, new_distvector$freqx,
  #      xlab = "theoretical",
  #      ylab = "empirical",
  #      main = "weibull")

  EnvStats::qqPlot(new_distvector$freqx, distribution = "weibull", estimate.params = T, add.line = TRUE, ylab = "empirical",main = "Weibull")

  #
  # if(dist_distribution=="gamma")
  # {
  emp_var <- new_distvector$freqx
  emp_shape <- (mean(emp_var)/stats::sd(emp_var))^(2)
  emp_scale <-stats::sd(emp_var)^2/mean(emp_var)

  y = stats::qgamma(stats::ppoints(length(emp_var)), shape = emp_shape, scale = emp_shape)

  # qqplot(y, new_distvector$freqx,
  #       xlab = "theoretical",
  #       ylab = "empirical",
  #       main = "gamma")

  EnvStats::qqPlot(new_distvector$freqx, distribution = "gamma", estimate.params = T, add.line = TRUE,  ylab = "empirical",main = "Gamma")

  #
  # if(dist_distribution=="chisq")
  #    {
  emp_var <- new_distvector$freqx
  emp_shape <- (mean(emp_var)/stats::sd(emp_var))^(2)
  emp_scale <-stats::sd(emp_var)^2/mean(emp_var)

  #      y = qchisq(stats::ppoints(length(emp_var)), df = length(prob)-1)
  # qqplot(y, new_distvector$freqx,
  #             xlab = "theoretical",
  #             ylab = "empirical",
  #             main = "chisq")
  #N <- unique(new_distvector$freqx)
  EnvStats::qqPlot(new_distvector$freqx, distribution = "chisq", param.list = list(df = 98), add.line = TRUE,  ylab = "empirical",main = "Chi-squared")

}

# qqPlot(new_distvector$freqx, distribution = "chisq", param.list = list(df = 551), add.line = TRUE)
#
# qqPlot(new_distvector$freqx, distribution = "norm", estimate.params = T, add.line = TRUE)
#
# fitdistr(new_distvector$freqx, "weibull")

