#' Plotting probability distributions across granularities
#'
#' Plot probability distribution of univariate series across bivariate temporal granularities.
#'
#' @param .data a tsibble.
#' @param response response variable.
#' @param gran1 the granularity which is to be placed across facets. Can be column names if required granularity already exists in the tsibble. For example, a column with public holidays which needs to be treated as granularity, can be included here.
#' @param gran2 the granularity to be placed across x-axis. Can be column names if required granularity already exists in the tsibble.
#' @param prob numeric vector of probabilities with values in [0,1].
#' @param hierarchy_tbl A hierarchy table specifying the hierarchy of units
#' @param dist_distribution Underlying distribution of distances. Look at hist_distance()
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
#' .data %>% histogram_distance(gran1 = "wknd_wday", gran2 = "hour_day", response = "general_supply_kwh")
#' .data %>% qqplot_distance(gran1 = "wknd_wday", gran2 = "hour_day", response = "general_supply_kwh")
#' .data %>% histogram_distance(gran1 = "day_week", gran2 = "hour_day", response = "general_supply_kwh")
#' .data %>% qqplot_distance(gran1 = "day_week", gran2 = "hour_day", response = "general_supply_kwh")
#' @export histogram_distance
#' @export qqplot_distance
# rank harmony table
hist_distance <- function(.data = NULL,
                         gran1 = NULL,
                         gran2 = NULL,
                         response = NULL,
                         prob = seq(0.01, 0.99, 0.01),
                         hierarchy_tbl = NULL,
                         dist_distribution = "normal")
{

data_gran <- create_gran_pair(.data,
                              gran1,
                              gran2,
                              hierarchy_tbl) %>%
      tibble::as_tibble() %>%
      dplyr::select(!!gran1, !!gran2, !!response)%>%
      tidyr::pivot_wider(names_from = !!gran1,
                         values_from = !!response,
                         values_fn = list(response = list))

   z <- dist_harmony_pair(data_gran, dist_distribution)
   distvector <- z$distvec

new_distvector <-
   distvector %>%
   tibble::as_tibble(.name_repair = "unique") %>%
   dplyr::mutate(newds = dplyr::row_number()) %>%
   tidyr::pivot_longer(-newds, names_to = "histx", values_to = "freqx")

new_distvector
}

histogram_distance <- function(.data = NULL,
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

new_distvector %>% ggplot2::ggplot(ggplot2::aes(x = freqx)) + ggplot2::geom_histogram(aes(y = ..density..)) + ggplot2::facet_grid(~newds) +
   ggplot2::xlab("pairwise distances") + ggplot2::ylab("relative frequencies") + geom_density(colour = "red")

}


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
                                   prob = seq(0.01, 0.99, 0.01))
dev.new()
par(mfrow = c(2, 2))
#
#    if(dist_distribution=="normal")
#    {
      emp_var <- new_distvector$freqx
      emp_shape <- (sd(emp_var)/ mean(emp_var))^(-1.086)
      emp_scale <- mean(emp_var)/gamma(1+1/emp_shape)

      y = qnorm(ppoints(length(emp_var)))
 # qqplot(y, new_distvector$freqx,
 #             xlab = "theoretical",
 #             ylab = "empirical",
 #             main = "normal")

      EnvStats::qqPlot(new_distvector$freqx, distribution = "norm", estimate.params = T, add.line = TRUE,  ylab = "empirical",main = "Normal")


#
# if(dist_distribution=="weibull")
# {
   emp_var <- new_distvector$freqx
   emp_shape <- (sd(emp_var)/ mean(emp_var))^(-1.086)
   emp_scale <- mean(emp_var)/gamma(1+1/emp_shape)

y = qweibull(ppoints(length(emp_var)), shape = emp_shape, scale = emp_scale)
  # qqplot(y, new_distvector$freqx,
  #      xlab = "theoretical",
  #      ylab = "empirical",
  #      main = "weibull")

EnvStats::qqPlot(new_distvector$freqx, distribution = "weibull", estimate.params = T, add.line = TRUE, ylab = "empirical",main = "Weibull")

#
# if(dist_distribution=="gamma")
# {
emp_var <- new_distvector$freqx
emp_shape <- (mean(emp_var)/sd(emp_var))^(2)
emp_scale <-sd(emp_var)^2/mean(emp_var)

y = qgamma(ppoints(length(emp_var)), shape = emp_shape, scale = emp_shape)

 # qqplot(y, new_distvector$freqx,
 #       xlab = "theoretical",
 #       ylab = "empirical",
 #       main = "gamma")

EnvStats::qqPlot(new_distvector$freqx, distribution = "gamma", estimate.params = T, add.line = TRUE,  ylab = "empirical",main = "Gamma")

#
# if(dist_distribution=="chisq")
#    {
      emp_var <- new_distvector$freqx
      emp_shape <- (mean(emp_var)/sd(emp_var))^(2)
      emp_scale <-sd(emp_var)^2/mean(emp_var)

 #      y = qchisq(ppoints(length(emp_var)), df = length(prob)-1)
 # qqplot(y, new_distvector$freqx,
 #             xlab = "theoretical",
 #             ylab = "empirical",
 #             main = "chisq")
      EnvStats::qqPlot(new_distvector$freqx, distribution = "chisq", param.list = list(df = 98), add.line = TRUE,  ylab = "empirical",main = "Chi-squared")

}

# qqPlot(new_distvector$freqx, distribution = "chisq", param.list = list(df = 551), add.line = TRUE)
#
# qqPlot(new_distvector$freqx, distribution = "norm", estimate.params = T, add.line = TRUE)
#
# fitdistr(new_distvector$freqx, "weibull")

