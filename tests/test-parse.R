context("parse")

test_that("parse is character",{
  expect_is(parse_exp("a"), "chr")})
