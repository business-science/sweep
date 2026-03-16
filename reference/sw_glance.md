# Construct a single row summary "glance" of a model, fit, or other object

Construct a single row summary "glance" of a model, fit, or other object

## Usage

``` r
sw_glance(x, ...)
```

## Arguments

- x:

  model or other R object to convert to single-row data frame

- ...:

  other arguments passed to methods

## Value

single-row tibble with model summary information.

## Details

`sw_glance()` is a wrapper for
[`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html).
The benefit of `sw_glance` is that it has methods for various
time-series model classes such as `HoltWinters`, `ets`, `Arima`, etc.
`sw_glance` methods always return either a one-row tibble or `NULL`. The
single row includes summary statistics relevent to the model accuracy,
which can be used to assess model fit and quality.

For non-time series, `sw_glance()` defaults to
[`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html).
The only difference is that the return is a tibble.

## See also

[`broom::glance()`](https://broom.tidymodels.org/reference/reexports.html)
