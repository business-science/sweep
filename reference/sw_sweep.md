# Tidy forecast objects

Tidy forecast objects

## Usage

``` r
sw_sweep(x, fitted = FALSE, timetk_idx = FALSE, rename_index = "index", ...)
```

## Arguments

- x:

  A time-series forecast of class `forecast`.

- fitted:

  Whether or not to return the fitted values (model values) in the
  results. FALSE by default.

- timetk_idx:

  If timetk index (non-regularized index) is present, uses it to develop
  forecast. Otherwise uses default index.

- rename_index:

  Enables the index column to be renamed.

- ...:

  Additional arguments passed to
  [`timetk::tk_make_future_timeseries()`](https://business-science.github.io/timetk/reference/tk_make_future_timeseries.html)

## Value

Returns a `tibble` object.

## Details

`sw_sweep` is designed to coerce `forecast` objects from the `forecast`
package into `tibble` objects in a "tidy" format (long). The returned
object contains both the actual values and the forecasted values
including the point forecast and upper and lower confidence intervals.

The `timetk_idx` argument is used to modify the return format of the
index.

- If `timetk_idx = FALSE`, a regularized time index is always
  constructed. This may be in the format of numeric values (e.g.
  2010.000) or the higher order `yearmon` and `yearqtr` classes from the
  `zoo` package. A higher order class is attempted to be returned.

- If `timetk_idx = TRUE` and a timetk index is present, an irregular
  time index will be returned that combines the original time series
  (i.e. date or datetime) along with a computed future time series
  created using
  [`timetk::tk_make_future_timeseries()`](https://business-science.github.io/timetk/reference/tk_make_future_timeseries.html)
  from the `timetk` package. The `...` can be used to pass additional
  arguments to
  [`timetk::tk_make_future_timeseries()`](https://business-science.github.io/timetk/reference/tk_make_future_timeseries.html)
  such as `inspect_weekdays`, `skip_values`, etc that can be useful in
  tuning the future time series sequence.

The index column name can be changed using the `rename_index` argument.

## See also

[`timetk::tk_make_future_timeseries()`](https://business-science.github.io/timetk/reference/tk_make_future_timeseries.html)

## Examples

``` r
library(forecast)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# ETS forecasts
USAccDeaths %>%
    ets() %>%
    forecast(level = c(80, 95, 99)) %>%
    sw_sweep()
#> # A tibble: 96 × 9
#>    index     key    value lo.80 lo.95 lo.99 hi.80 hi.95 hi.99
#>    <yearmon> <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Jan 1973  actual  9007    NA    NA    NA    NA    NA    NA
#>  2 Feb 1973  actual  8106    NA    NA    NA    NA    NA    NA
#>  3 Mar 1973  actual  8928    NA    NA    NA    NA    NA    NA
#>  4 Apr 1973  actual  9137    NA    NA    NA    NA    NA    NA
#>  5 May 1973  actual 10017    NA    NA    NA    NA    NA    NA
#>  6 Jun 1973  actual 10826    NA    NA    NA    NA    NA    NA
#>  7 Jul 1973  actual 11317    NA    NA    NA    NA    NA    NA
#>  8 Aug 1973  actual 10744    NA    NA    NA    NA    NA    NA
#>  9 Sep 1973  actual  9713    NA    NA    NA    NA    NA    NA
#> 10 Oct 1973  actual  9938    NA    NA    NA    NA    NA    NA
#> # ℹ 86 more rows

```
