
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gravitas

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/Sayani07/gravitas.svg?branch=master)](https://travis-ci.org/Sayani07/gravitas)
<!-- badges: end -->

The goal of gravitas is to …

## Installation

You can install the released version of gravitas from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("gravitas")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Sayani07/gravitas")
```

## Example

This is an example which shows how to create any temporal granularity.

``` r
library(gravitas)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
tsibbledata::nyc_bikes %>% 
  tail() %>%
  mutate(hhour_week = build_gran("hhour", "week", start_time), 
         hour_day = build_gran("hour", "day", start_time)) %>%
  select(bike_id, start_time, start_station, end_station, hhour_week, hour_day)
#> # A tibble: 6 x 6
#>   bike_id start_time          start_station end_station hhour_week hour_day
#>   <fct>   <dttm>              <fct>         <fct>            <dbl>    <int>
#> 1 33571   2018-11-01 18:52:10 3195          3210               228       18
#> 2 33571   2018-11-02 08:50:04 3210          3640               256        8
#> 3 33571   2018-11-02 17:52:14 3640          3196               274       17
#> 4 33571   2018-11-04 08:12:56 3196          3269                15        8
#> 5 33571   2018-11-04 14:27:17 3269          3202                27       14
#> 6 33571   2018-11-04 18:37:04 3202          3187                36       18
```
