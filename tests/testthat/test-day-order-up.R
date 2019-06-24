context("day order functions")

x = lubridate::ymd_hms("2014-12-30 23:31:15 UTC")


test_that("second_minute inputs", {
  expect_is(x,c("POSIXct", "POSIXt"))
})


#day_fortnight
test_that("day_fortnight outputs a numeric value", {
  expect_is(day_fortnight(x), "numeric")
})

test_that("day_fortnight expected output", {
  expect_equal(day_fortnight(x), 14)
})

test_that("day_fortnight output length equals input length", {
  expect_length(day_fortnight(x), length(x))
})

test_that("day_fortnight error with null input", {
  expect_error(day_fortnight(), "argument \"x\" is missing, with no default")
})


#day_semester

test_that("day_semester outputs a numeric value", {
  expect_is(day_semester(x), "numeric")
})

test_that("day_semester expected output", {
  expect_equal(day_semester(x), 183)
})

test_that("day_semester output length equals input length", {
  expect_length(day_semester(x), length(x))
})

test_that("day_semester error with null input", {
  expect_error(day_semester(), "argument \"x\" is missing, with no default")
})

