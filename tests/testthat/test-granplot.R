context("creates plot")

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

test_that("granplot error with null input", {
  expect_error(granplot(), "argument \".data\" is missing, with no default")
})


test_that("tsibble input", {
  expect_is(x, c("tbl_ts", "tbl_df", "tbl", "data.frame"))
})

test_that("character output", {
  expect_is(granplot(x, "hour_day", "day_week"), c("gg", "ggplot"))
})


test_that("throws error when granularities to be plotted are not specified", {
  expect_error(granplot(x), "Specify the granularities that are to be plotted")
})


test_that("throws error with just one granularity", {
  expect_error(granplot(x, gran2 = "hour_day"), "Specify the granularities that are to be plotted")
})


test_that("throws error with no hierarchy table specified for non temporal data", {
  expect_error(granplot(cricket_tsibble, "ball_over", "over_inning"), "Hierarchy table must be provided when class of index of the tsibble is not date-time")
})

test_that("throws error with incorrect input for granularities", {
  expect_error(granplot(cricket_tsibble, "balls_over", "over_inning", hierarchy_model), "lower part of granularity must be listed as an element in the hierarchy table")
})
