context("build_gran")

x = "2018-11-04 18:37:04 EST"

test_that("build_gran inputs", {
  expect_is(x,c("POSIXct", "POSIXt"))
})


test_that("build_gran outputs a numeric value", {
  expect_is(build_gran("hour", "week", x), "numeric")
})

test_that("build_gran output length equals input length", {
  expect_length(build_gran(x), length(x))
})


test_that("build_gran expected output", {
  expect_equal(build_gran(x, "hour", "week"), 18)
})

test_that("build_gran error with null input", {
  expect_error(build_gran(), "argument \"x\" is missing, with no default")
})
