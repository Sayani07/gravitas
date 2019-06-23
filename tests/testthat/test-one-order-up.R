context("one order up functions")

x = lubridate::ymd_hms("2014-12-31 23:31:15 UTC")


test_that("second_minute inputs", {
  expect_is(x,c("POSIXct", "POSIXt"))
})



# second_minute

test_that("second_minute outputs a numeric value", {
  expect_is(second_minute(x), "numeric")
})

test_that("second_minute expected output", {
  expect_is(minute_qhour(x), 15)
})



# minute_qhour
test_that("minute_qhour outputs a numeric value", {
  expect_is(minute_qhour(x), "numeric")
})


test_that("minute_qhour expected output", {
  expect_equal(minute_qhour(x), 2)
})


#qhour_hhour

test_that("qhour_hhour outputs a numeric value", {
  expect_is(qhour_hhour(x), "numeric")
})


test_that("qhour_hhour expected output", {
  expect_equal(qhour_hhour(x), 1)
})


#hhour_hour



test_that("hhour_hour outputs a numeric value", {
  expect_is(hhour_hour(x), "numeric")
})


test_that("hhour_hour expected output", {
  expect_equal(hhour_hour(x), 2)
})

#week_fortnight


test_that("week_fortnight outputs a numeric value", {
  expect_is(week_fortnight(x), "numeric")
})


test_that("week_fortnight expected output", {
  expect_equal(week_fortnight(x), 2)
})


