#' Smart meter data for ten households
#'
#' Customer Trial data conducted as part of
#' Smart Grid Smart City (SGSC) project
#' (2010-2014) based in Newcastle,
#' New South Wales
#' and areas in Sydney.
#' It contains half hourly interval meter readings (KWh)
#' of electricity consumption and generation of households.
#' @format A tsibble with 259, 235 rows and 3 columns:
#' * **customer_id**: household ID
#' * **reading_datetime**: Date time for which data is recorded (index)
#' * **general_supply_kwh**:electricity supplied to this household
#' @references [Department of the Environment and Energy, Australia](https://data.gov.au/dataset/ds-dga-4e21dea3-9b87-4610-94c7-15a8a77907ef/details?q=smart-meter)
#' @name smart_meter10
#' @usage data(smart_meter10)
"smart_meter10"



#' Cricket data set for different seasons of Indian Premier League
#'
#' The Indian Premier League played by teams representing
#' different cities in India from 2008 to 2016
#'
#' @format A tibble with 8560 rows and 11 variables:
#' **season**: years representing IPL season
#' **match_id**: match codes
#' **batting_team**: name of batting team
#' **bowling_team**: name of bowling team
#' **inning**: innings of the match
#' **over**: overs of the inning
#' **wicket**: number of wickets in each over
#' **dot_balls**: number of balls with no runs in an over
#' **runs_per_over**: Runs for each over
#' **run_rate**: run rate for each over
#' @references [Kaggle IPL Data Analysis](https://www.kaggle.com/josephgpinto/ipl-data-analysis/data)
#' @name cricket
#' @examples
#' data(cricket)
#'
#'library(tsibble)
#'library(dplyr)
#'library(ggplot2)
#'
#' # convert data set to a tsibble ----
#' cricket_tsibble <- cricket %>%
#'   mutate(data_index = row_number()) %>%
#'   as_tsibble(index = data_index)
#' # set the hierarchy of the units in a table ----
#' hierarchy_model <- tibble::tibble(
#'   units = c("index", "ball", "over", "inning", "match"),
#'   convert_fct = c(1, 6, 20, 2, 1))
#'# Compute granularities ----
#' cricket_tsibble %>%
#'   create_gran("over_inning",
#'                hierarchy_model)
#' # Visualise distribution of runs across granularities ----
#' cricket_tsibble %>%
#'   filter(batting_team %in% c("Mumbai Indians",
#'                              "Chennai Super Kings"))%>%
#'   prob_plot("inning", "over",
#'   hierarchy_model,
#'   response = "runs_per_over",
#'   plot_type = "lv")

"cricket"
