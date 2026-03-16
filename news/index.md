# Changelog

## sweep 0.2.5

CRAN release: 2023-07-06

- Fixes to get `sweep` back on CRAN following inadvertent `timetk`
  archival.

## sweep 0.2.4

- Remove support for `robets`.

## sweep 0.2.3

CRAN release: 2020-07-10

- Fixes for compatability with `broom` v0.7.0
- Add tidiers for
  [`stlm()`](https://pkg.robjhyndman.com/forecast/reference/stlm.html)
  models

## sweep 0.2.2

CRAN release: 2019-10-08

- Fixes for compatability with `tidyquant` v0.5.7

## sweep 0.2.1

CRAN release: 2018-03-03

- Fixes for
  [`forecast::mstl`](https://pkg.robjhyndman.com/forecast/reference/mstl.html)

## sweep 0.2.0

CRAN release: 2017-07-26

- Change to `timetk` from `timekit`.
- Fix Issue [\#2](https://github.com/business-science/sweep/issues/2) -
  `sw_tidy` fails when
  [`auto.arima()`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
  returns no terms (coefficients).

## sweep 0.1.0

CRAN release: 2017-07-03

- Initial release of `sweep`, a tool to “tidy” the forecast modeling and
  prediction workflow.
