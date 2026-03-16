# Tidy the result of a time-series model into a summary tibble

Tidy the result of a time-series model into a summary tibble

## Usage

``` r
sw_tidy(x, ...)
```

## Arguments

- x:

  An object to be converted into a tibble ("tidy" data.frame)

- ...:

  extra arguments

## Value

a tibble

## Details

`sw_tidy()` is a wrapper for
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html).
The main benefit of `sw_tidy()` is that it has methods for various
time-series model classes such as `HoltWinters`, `ets`, `Arima`, etc.
`sw_tidy()` methods always returns a "tidy" tibble with model
coefficient / parameters.

For non-time series, `sw_tidy()` defaults to
[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html).
The only difference is that the return is a tibble. The output of
`sw_tidy()` is always a tibble with disposable row names. It is
therefore suited for further manipulation by packages like dplyr and
ggplot2.

## See also

[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)

## Examples

``` r
library(dplyr)
library(forecast)

WWWusage %>%
    auto.arima() %>%
    sw_tidy(conf.int = TRUE)
#> # A tibble: 2 × 2
#>   term  estimate
#>   <chr>    <dbl>
#> 1 ar1      0.650
#> 2 ma1      0.526
```
