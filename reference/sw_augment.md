# Augment data according to a tidied model

Given an R statistical model or other non-tidy object, add columns to
the original dataset such as predictions, residuals and cluster
assignments.

## Usage

``` r
sw_augment(x, ...)
```

## Arguments

- x:

  model or other R object to convert to data frame

- ...:

  other arguments passed to methods

## Details

`sw_augment()` is a wrapper for
[`broom::augment()`](https://broom.tidymodels.org/reference/reexports.html).
The benefit of `sw_augment` is that it has methods for various
time-series model classes such as `HoltWinters`, `ets`, `Arima`, etc.

For non-time series, `sw_augment()` defaults to
[`broom::augment()`](https://broom.tidymodels.org/reference/reexports.html).
The only difference is that the return is a tibble.

Note that by convention the first argument is almost always `data`,
which specifies the original data object. This is not part of the S3
signature, partly because it prevents rowwise_df_tidiers from taking a
column name as the first argument.

## See also

[`broom::augment()`](https://broom.tidymodels.org/reference/reexports.html)
