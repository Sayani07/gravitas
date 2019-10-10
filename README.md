
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gravitas

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/Sayani07/gravitas.svg?branch=master)](https://travis-ci.org/Sayani07/gravitas)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Sayani07/gravitas?branch=master&svg=true)](https://ci.appveyor.com/project/Sayani07/gravitas)
<!-- badges: end -->

The package **gravitas** provides a tool to examine the probability
distribution of univariate time series across bivariate temporal
granularities using range of graphics in `ggplot2` through the
following:

  - create multiple-order-up circular or aperiodic temporal
    granularities.

  - categorize pairs of granularities as either *harmony* or *clash*,
    where harmonies are pairs of granularities that aid exploratory data
    analysis, and clashes are pairs that are incompatible with each
    other for exploratory analysis.

  - recommending appropriate probability distribution plots of the time
    series variable across the bivariate granularities based on the
    levels of the bivariate granularties and their compatibility
    relationship.

## Installation

<!-- You can install the released version of gravitas from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("gravitas") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Sayani07/gravitas")
```

## Quick look

gravitas comes with an interactive webpage, which lets you go through
the different functionalities of this package. To try it, simply use
gravitas::run\_app().

## Features

  - Build any temporal granularity with `create_gran`

  - Get set of possible temporal granularities with `search_gran()`
    
      - Refine your search of possible temporal granularities by
        altering arguments in `search_gran()`

  - Check if two temporal granularities are harmonies with
    `is.harmony()`

  - Get all possible harmonies with `harmony()`

  - Explore probability distribution across bivariate temporal
    granularities with `granplot()`
    
      - Get recommendations on choosing more appropriate distribution
        plots

## More information

View the vignette to get started\!

``` r
library("gravitas")
#vignette("gravitas_vignette")
```

You can force building the vignettes with

``` r
#devtools::install_github("Sayani07/gravitas", build_vignettes = TRUE)
```

This package takes tsibble as the data input. Tsibble provides a data
class of tbl\_ts to represent tidy temporal data. It consists of a time
index, key and other measured variables in a data-centric format, which
makes it easier to work with temporal data. To learn more about it,
please visit <https://tsibble.tidyverts.org/>

## Reporting and issues

Please submit all bug reports, errors, and feature requests to
<https://github.com/Sayani07/gravitas/issues>
