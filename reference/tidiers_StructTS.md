# Tidying methods for StructTS (Error, Trend, Seasonal) / exponential smoothing modeling of time series

These methods tidy the coefficients of StructTS models of univariate
time series.

## Usage

``` r
# S3 method for class 'StructTS'
sw_tidy(x, ...)

# S3 method for class 'StructTS'
sw_glance(x, ...)

# S3 method for class 'StructTS'
sw_augment(x, data = NULL, timetk_idx = FALSE, rename_index = "index", ...)
```

## Arguments

- x:

  An object of class "StructTS"

- ...:

  Additional parameters (not used)

- data:

  Used with `sw_augment` only. `NULL` by default which simply returns
  augmented columns only. User can supply the original data, which
  returns the data + augmented columns.

- timetk_idx:

  Used with `sw_augment` only. Uses a irregular timetk index if present.

- rename_index:

  Used with `sw_augment` only. A string representing the name of the
  index generated.

## Value

**[`sw_tidy()`](https://business-science.github.io/sweep/reference/sw_tidy.md)**
returns one row for each model parameter, with two columns:

- `term`: The model parameters

- `estimate`: The estimated parameter value

**[`sw_glance()`](https://business-science.github.io/sweep/reference/sw_glance.md)**
returns one row with the columns

- `model.desc`: A description of the model including the three integer
  components (p, d, q) are the AR order, the degree of differencing, and
  the MA order.

- `sigma`: The square root of the estimated residual variance

- `logLik`: The data's log-likelihood under the model

- `AIC`: The Akaike Information Criterion

- `BIC`: The Bayesian Information Criterion

- `ME`: Mean error

- `RMSE`: Root mean squared error

- `MAE`: Mean absolute error

- `MPE`: Mean percentage error

- `MAPE`: Mean absolute percentage error

- `MASE`: Mean absolute scaled error

- `ACF1`: Autocorrelation of errors at lag 1

**[`sw_augment()`](https://business-science.github.io/sweep/reference/sw_augment.md)**
returns a tibble with the following time series attributes:

- `index`: An index is either attempted to be extracted from the model
  or a sequential index is created for plotting purposes

- `.actual`: The original time series

- `.fitted`: The fitted values from the model

- `.resid`: The residual values from the model

## See also

[`stats::StructTS()`](https://rdrr.io/r/stats/StructTS.html)

## Examples

``` r
library(dplyr)
library(forecast)

fit_StructTS <- WWWusage %>%
    StructTS()

sw_tidy(fit_StructTS)
#> # A tibble: 3 × 2
#>   term    estimate
#>   <chr>      <dbl>
#> 1 level        0  
#> 2 slope       13.0
#> 3 epsilon      0  
sw_glance(fit_StructTS)
#> # A tibble: 1 × 12
#>   model.desc      sigma logLik   AIC   BIC      ME  RMSE   MAE   MPE  MAPE  MASE
#>   <chr>           <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Local linear s… 0.995  -277.  559.  564. -0.0200  3.59  2.96 0.140  2.32 0.654
#> # ℹ 1 more variable: ACF1 <dbl>
sw_augment(fit_StructTS)
#> # A tibble: 100 × 4
#>    index .actual .fitted .resid
#>    <int>   <dbl>   <dbl>  <dbl>
#>  1     1      88    88     0   
#>  2     2      84    88.0  -4.00
#>  3     3      85    80     5   
#>  4     4      85    86    -1   
#>  5     5      84    85    -1   
#>  6     6      85    83     2   
#>  7     7      83    86    -3   
#>  8     8      85    81     4   
#>  9     9      88    87     1   
#> 10    10      89    91    -2   
#> # ℹ 90 more rows
```
