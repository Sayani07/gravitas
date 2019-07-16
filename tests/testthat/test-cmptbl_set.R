context("comptbl_set")

x <- lubridate::ymd_hms("2018-11-04 18:37:04 EST")

.data1 <- tsibbledata::nyc_bikes %>% tsibble::as_tsibble()
.data2 <- tsibbledata::vic_elec %>% tsibble::as_tsibble()

expected <- tibble(granularities = c("hhour_hour", "hour_day", "hhour_day"), hhour_hour = c(0, 1, 1), hhour_day = c(0, 0, 0), hour_day = c(1, 0, 0))



test_that("comptbl_set error for no input data", {
  expect_error(comp_tbl(ugran = "week"), "argument \".data\" is missing, with no default")
})

test_that("comptbl_set error for no ugran data", {
  expect_error(comp_tbl(.data1), "Argument ugran is missing, with no default")
})


test_that("comptbl_set error for no lgran in irregular spaced data", {
  expect_error(comp_tbl(.data1, ugran = "week"), "lgran must be provided when the tsibble is irregularly spaced")
})


test_that("expected class for comptbl_set", {
  expect_is(comp_tbl(.data2, ugran = "day"), c("tbl_df", "tbl", "data.frame"))
})
