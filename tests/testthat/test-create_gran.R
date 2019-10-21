context("create granularities")

x <- tsibbledata::vic_elec

cricket_tsibble <- cricket %>%
  dplyr::mutate(data_index = dplyr::row_number()) %>%
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

test_that("tsibble input", {
  expect_is(x, c("tbl_ts", "tbl_df", "tbl", "data.frame"))
})

test_that("create_gran error with null input", {
  expect_error(create_gran(), "argument \".data\" is missing, with no default")
})

test_that("create_gran throws error with
          no granularity specified", {
  expect_error(create_gran(x),
               "Provide the granularity that\n         needs to be computed")
})



test_that("tsibble output", {
  expect_is(create_gran(x,
                        "hour_day"),
            c("tbl_ts", "tbl_df", "tbl", "data.frame"))
})

test_that("create grans creates a factor", {
  expect_is(create_gran(x, "hhour_week") %>%
              as_tibble() %>%
              .$hhour_week,
            "factor")
})

test_that("create_gran error with incorrect
          input for lower part of granularity", {
  expect_error(create_gran(x,
                           "hours_day"),
               "lower part of granularity must
           be listed as an element in the hierarchy table")
})

test_that("create_gran error with incorrect
          input for upper part of granularity", {
  expect_error(create_gran(x,
                           "hour_days"),
               "upper part of granularity must
           be listed as an element in the hierarchy table")
})

# non-temporal data

test_that("create grans creates a
          factor for non-temporal data", {
  expect_is(create_gran(cricket_tsibble,
                        "ball_inning",
                        hierarchy_tbl = hierarchy_model) %>%
              as_tibble() %>%
              .$ball_inning,
            "factor")
})



test_that("create grans throws error
          if hierarchy table no provided for non-temporal data", {
  expect_error(
    create_gran(cricket_tsibble,
                "ball_inning"),
    "Hierarchy table must be provided\n           when class of index of the tsibble\n           is not date-time"
  )
})


test_that("create grans throws error if
          incorrect input for lower part of gran", {
  expect_error(
    create_gran(cricket_tsibble,
                "balls_inning",
                hierarchy_model),
    "lower part of granularity must be\n           listed as an element in the hierarchy table"
  )
})

test_that("create grans throws error
          if incorrect input for upper part of gran", {
  expect_error(
    create_gran(cricket_tsibble,
                "ball_innings",
                hierarchy_model),
    "upper part of granularity must be\n           listed as an element in the hierarchy table"
  )
})
