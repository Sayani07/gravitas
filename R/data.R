#' Smart meter data for a household
#'
#' Customer Trial data conducted as part of
#' Smart Grid Smart City (SGSC) project
#' (2010-2014) based in Newcastle,
#' New South Wales
#' and areas in Sydney.
#' It contains half hourly interval meter readings (KWh)
#' of electricity consumption and generation of households.
#' The data is obtained from url{https://data.gov.au/dataset/ds-dga-4e21dea3-9b87-4610-94c7-15a8a77907ef/details?q=smart-meter}
#'
#' @usage data(smart_meter10)
#' @format A tsibble with 3 variables
#' \describe{
#'   \item{customer_id}{household ID}
#'   \item{reading_datetime}{timestamps for which data is collected}
#'   \item{general_supply_KwH}{electricity supplied to this household}
#' }
"smart_meter10"


#' Cricket data for IPL 2008
#'
#' The Indian Premiere League played
#' by eight teams representing
#' different cities in India for 2008.
#' The data is obtained from url{https://www.kaggle.com/littleraj30/indian-premier-league-2019-ball-by-ball/version/1}
#'
#' @usage data(cricketdata)
"cricketdata"

#' Cricket data IPL season 2008 to 2016
#'
#' The Indian Premiere League played
#' by teams representing
#' different cities in India conisting of match_id, batting and bowling #' team, innings, over, runs per overs and rate rate per over
#' The data is obtained from url{https://www.kaggle.com/littleraj30
#' /indian-premier-league-2019-ball-by-ball/version/1}
#'
#' @usage data(cricket)
"cricket"
