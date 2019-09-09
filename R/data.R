#' Smart meter data for an household
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
#' @usage data(sm_cust1)
#' @format A tsibble with 3 variables
#' \describe{
#'   \item{customer_id}{household ID}
#'   \item{reading_datetime}{timestamps for which data is collected}
#'   \item{general_supply_KwH}{electricity supplied to this household}
#' }
"sm_cust1"

#' Smart meter data for 50 households
#'
#' Customer Trial data conducted as part of
#' Smart Grid Smart City (SGSC) project
#' (2010-2014) based in Newcastle,
#' New South Wales
#' and areas in Sydney.
#' It contains half hourly interval meter readings (KWh)
#' of electricity consumption
#' and generation of households.
#' The data is obtained from url{https://data.gov.au/dataset/ds-dga-4e21dea3-9b87-4610-94c7-15a8a77907ef/details?q=smart-meter}
#'
#' @usage data(sm_cust50)
"sm_cust50"

