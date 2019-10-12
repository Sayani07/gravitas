context("recommendation on plots and check points")

x <- tsibbledata::vic_elec

cricket_tsibble <- cricketdata %>%
  dplyr::mutate(data_index = row_number()) %>%
  tsibble::as_tsibble(index = data_index)

hierarchy_model <- tibble::tibble(
  units = c(
    "index",
    "ball",
    "over",
    "inning",
    "match"
  ),
  convert_fct = c(1, 6, 20, 2, 1)
)

# common tests for temporal and non-temporal data

test_that("prob_plot error with null input", {
  expect_error(gran_advice(),  "argument \".data\" is missing, with no default")
})


test_that("tsibble input", {
  expect_is(x, c("tbl_ts",
                 "tbl_df",
                 "tbl",
                 "data.frame"))
})

test_that("list output", {
  expect_is(gran_advice(x,
                        "hour_day",
                        "hhour_hour"), c("list"))
})


test_that("throws error when granularities
          to be plotted are not specified", {
  expect_error(gran_advice(x),
               "argument \"gran1\" is missing, with no default")
})


test_that("throws error with just one granularity", {
  expect_error(gran_advice(x,
                           gran2 = "hour_day"),
               "argument \"gran1\" is missing, with no default")
})


test_that("throws error with no hierarchy table
          specified for non temporal data", {
  expect_error(gran_advice(cricket_tsibble,
                           "ball_over",
                           "over_inning"),
               "Hierarchy table must be provided\n           when class of index of the tsibble\n           is not date-time")
})

test_that("throws error with incorrect input for granularities", {
  expect_error(gran_advice(cricket_tsibble,
                           "balls_over",
                           "over_inning",
                           hierarchy_model),
               "lower part of granularity must be\n           listed as an element in the hierarchy table")
})
