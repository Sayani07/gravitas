#' Runs a shiny app demonstrating functionalities of gravitas
#'
#' Runs a local shiny app that demonstrates how distributions of univariate #' time series can be explored across bivariate time granularities
#'
#' @author Sayani Gupta
#' @return opens a local shiny app
#'@examples
#'\dontrun{
#' run_app()
#'}
#' @export

# For adjusting or adding more apps it may be useful to follow:
# https://deanattali.com/2015/04/21/r-package-shiny-app/
run_app <- function() {
  appDir <- system.file("shiny-examples",
                        "gravitas_app",
                        package = "gravitas")
  if (appDir == "") {
    stop("Could not find example directory.
         Try re-installing `gravitas`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
