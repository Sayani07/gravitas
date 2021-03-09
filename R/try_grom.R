library(ggplot2)
library(tidyverse)

p <- ggplot(mpg, aes(class, hwy))
p + geom_quantile()
#
# m <- ggplot(mpg, aes(displ, 1 / hwy))
# m + geom_quantile()

# stat-quantileplot
StatHdrcde <- ggproto("Statquantileplot", Stat,
                      required_aes = c("y", "x"),
                      # non_missing_aes = "weight",

                      #setup_params = ggplot2::StatBoxplot$setup_params,

                      setup_data = function(data, params) {
                        # How are missing values handled?
                        data$x <- data$x %||% 0
                        data$y <- data$y %||% 0
                        data
                      },

                      compute_group = function(data, scales, width = NULL, probs = NULL, all.modes = TRUE, na.rm = FALSE) {
                        if (length(unique(data$x)) > 1)
                          width <- diff(range(data$x)) * 0.9

                        if(!all(data$x == data$x[1])){
                          #stop("Conditional density estimation is not yet supported. Make sure each plot group contains only one x value.")

                          hdrcde_est <- hdrcde::cde(data$x, data$y)
                          hdr_stats <- hdrcde::hdr.cde(hdrcde_est, prob = probs*100, plot = F)

                          one_cde_row <- function(idx, hdrcde_est, hdr_stats) {
                            hdr <- t(hdr_stats$hdr[[idx]])
                            max_boxes <- ncol(hdr)/2

                            df <- structure(list(), .Names = character(0), row.names = c(NA, -1L), class = "data.frame")
                            box <- split(hdr, col(hdr) %% 2)
                            is_box <- complete.cases(box)
                            df$prob <- list(rep(sort(probs, decreasing = TRUE), max_boxes))
                            df$box <- list(matrix(
                              c(box[[2]], box[[1]]), ncol = 2,
                              dimnames = list(NULL, c("lower", "upper"))
                            ))
                            df$ymax <- max(box[[1]], na.rm = TRUE)
                            df$ymin <- min(box[[2]], na.rm = TRUE)

                            df$mode <- list(hdr_stats$modes[idx,1])
                            df$width <- hdrcde_est$x[2] - hdrcde_est$x[1]

                            df$x <- hdrcde_est$x[idx]
                            df$x_cde <- hdrcde_est$x[idx]
                            df
                          }

                          do.call(rbind, lapply(seq_along(hdrcde_est$x), one_cde_row, hdrcde_est, hdr_stats))

                        } else {
                          # imported from hdrcde
                          hdr_stats <- hdrcde::hdr(data$y, prob = probs*100, all.modes = all.modes)

                          hdr <- hdr_stats$hdr

                          # number of boxes (for all probabilities max number of boxes will be shown although it has got NA values)
                          max_boxes <- ncol(hdr)/2

                          # initialise 1 row data.frame
                          df <- structure(list(), .Names = character(0), row.names = c(NA, -1L), class = "data.frame")

                          box <- split(hdr, col(hdr) %% 2)
                          is_box <- complete.cases(box)
                          df$prob <- list(rep(sort(probs, decreasing = TRUE), amax_boxes)[is_box])
                          df$box <- list(matrix(
                            c(box[[2]], box[[1]]), ncol = 2,
                            dimnames = list(NULL, c("lower", "upper"))
                          )[is_box,])
                          df$ymax <- max(box[[1]], na.rm = TRUE)
                          df$ymin <- min(box[[2]], na.rm = TRUE)

                          # keep only modes within boxes
                          mode_proxy <- matrix(0, length(hdr_stats$mode), df$ymax)
                          for(i in 1:length(hdr_stats$mode))
                          {
                            for(j in 1:length(df$ymax))
                              if(hdr_stats$mode[i]<=df$ymax[j] & hdr_stats$mode[i]>= df$ymin[j])
                              {
                                mode_proxy[i, j] <- 0
                              }
                            else mode_proxy[i, j] <- 1
                          }
                          hdr_mode <- hdr_stats$mode[which(rowSums(mode_proxy)==0)]
                          df$mode <- list(hdr_mode)

                          df$width <- width
                          df$x <- unique(data$x) # FIX LATER
                          df
                        }


                      }
)



