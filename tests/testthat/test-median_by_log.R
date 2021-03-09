context("median by log of distances")

x <- list(1:10)

test_that("list input", {
  expect_is(x, c("list"))
})


test_that("positive length of vector", {
  expect_gt(length(unlist(x)), 1)
})
