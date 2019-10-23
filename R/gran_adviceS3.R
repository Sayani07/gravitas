#define the S3 method with USeMethod


#' @examples
#' library(tsibbledata)
#' .data <- tsibbledata::vic_elec
#'  gran1 <-  "hour_day"
#'  gran2 <- "day_month"

p = gran_advice(.data, gran1, gran2)

#print <- function(x) UseMethod("print")


print.gran_advice_s3 <- function(object){

  z <- object

   cat("Recommended plots:", z$plot_choices, "\n")
   cat("gran_obs:")
   z$gran_obs
   print(z$gran_obs)

  ans <- NULL
  # ans$harmony <-  cat("Harmony:", z$harmony,  "\n")
  # ans$homogenous <-  z$homogenous
  # ans$gran_obs <- z$gran_obs

  #ans <- z[c("harmony", "homogenous", "plot_choices")]
  #gran_obs <- z$gran_obs
  #harmony <- z$harmony
  #ans <-  list(gran_obs = gran_obs, harmony)
  ans <- cat("Recommended plot(s) include", z$plot_choices, "/n")
  #names(gran_obs) <- "gran_obs"
  #class(ans) <- "advice.gran_advice_s3"
  z

}


gran_advice_s3 <- function(.data,
                        gran1,
                        gran2,
                        hierarchy_tbl = NULL,
                        ...) {

  # data must be tsibble
  if (!tsibble::is_tsibble(.data)) {
    stop("must use tsibble")
  }

  if (is.null(gran1) | is.null(gran2)) {
    stop("Specify the granularities that are to be plotted")
  }

  # checking if input data is harmony
  harmony <- is_harmony(.data,
                        gran1,
                        gran2,
                        hierarchy_tbl
  )

  homogenous <- is_homogenous(.data,
                              gran1,
                              gran2,
                              hierarchy_tbl
  )

  plot_choices <- plot_choices(.data,
                               gran1,
                               gran2,
                               hierarchy_tbl,
                               facet_h = 31,
                               facet_m = 14,
                               facet_l = 7,
                               x_h = 31,
                               x_m = 14,
                               x_l = 7,
                               ...
  )

  gran_obs <- gran_obs(.data,
                       gran1,
                       gran2,
                       hierarchy_tbl,
                       ...
  )

  z <- list(harmony = harmony,
            homogenous = homogenous,
            plot_choices = plot_choices,
            gran_obs = gran_obs)

  class(z) <- "gran_advice_s3"
  z
}




