context("gran_convert")

test_that("gran_convert is numeric",{
  expect_is(gran_convert("hour", "day"), "numeric")})

test_that("gran_convert has length 1",{
  expect_equal(length(gran_convert("hour", "day")), 1)})

