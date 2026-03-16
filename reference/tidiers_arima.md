# Tidying methods for ARIMA modeling of time series

These methods tidy the coefficients of ARIMA models of univariate time
series.

## Usage

``` r
# S3 method for class 'Arima'
sw_tidy(x, ...)

# S3 method for class 'Arima'
sw_glance(x, ...)

# S3 method for class 'Arima'
sw_augment(x, data = NULL, rename_index = "index", timetk_idx = FALSE, ...)

# S3 method for class 'stlm'
sw_tidy(x, ...)
```

## Arguments

- x:

  An object of class "Arima"

- ...:

  Additional parameters (not used)

- data:

  Used with `sw_augment` only. `NULL` by default which simply returns
  augmented columns only. User can supply the original data, which
  returns the data + augmented columns.

- rename_index:

  Used with `sw_augment` only. A string representing the name of the
  index generated.

- timetk_idx:

  Used with `sw_augment` only. Uses a irregular timetk index if present.

## Value

**[`sw_tidy()`](https://business-science.github.io/sweep/reference/sw_tidy.md)**
returns one row for each coefficient in the model, with five columns:

- `term`: The term in the nonlinear model being estimated and tested

- `estimate`: The estimated coefficient

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

**[`sw_tidy()`](https://business-science.github.io/sweep/reference/sw_tidy.md)**
returns the underlying ETS or ARIMA model's
[`sw_tidy()`](https://business-science.github.io/sweep/reference/sw_tidy.md)
one row for each coefficient in the model, with five columns:

- `term`: The term in the nonlinear model being estimated and tested

- `estimate`: The estimated coefficient

## See also

[`stats::arima()`](https://rdrr.io/r/stats/arima.html)

## Examples

``` r
library(dplyr)
library(forecast)

fit_arima <- WWWusage %>%
    auto.arima()

sw_tidy(fit_arima)
#> # A tibble: 2 × 2
#>   term  estimate
#>   <chr>    <dbl>
#> 1 ar1      0.650
#> 2 ma1      0.526
sw_glance(fit_arima)
#> # A tibble: 1 × 12
#>   model.desc   sigma logLik   AIC   BIC    ME  RMSE   MAE   MPE  MAPE  MASE
#>   <chr>        <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 ARIMA(1,1,1)  3.16  -254.  514.  522. 0.304  3.11  2.41 0.281  1.92 0.532
#> # ℹ 1 more variable: ACF1 <dbl>
sw_augment(fit_arima)
#> # A tibble: 100 × 4
#>    index .actual .fitted  .resid
#>    <int>   <dbl>   <dbl>   <dbl>
#>  1     1      88    87.9  0.0880
#>  2     2      84    86.2 -2.17  
#>  3     3      85    81.1  3.86  
#>  4     4      85    87.5 -2.45  
#>  5     5      84    83.7  0.259 
#>  6     6      85    83.5  1.51  
#>  7     7      83    86.4 -3.44  
#>  8     8      85    79.9  5.11  
#>  9     9      88    89.0 -0.985 
#> 10    10      89    89.4 -0.433 
#> # ℹ 90 more rows

```
