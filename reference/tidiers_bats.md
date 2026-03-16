# Tidying methods for BATS and TBATS modeling of time series

Tidying methods for BATS and TBATS modeling of time series

## Usage

``` r
# S3 method for class 'bats'
sw_tidy(x, ...)

# S3 method for class 'bats'
sw_glance(x, ...)

# S3 method for class 'bats'
sw_augment(x, data = NULL, rename_index = "index", timetk_idx = FALSE, ...)

# S3 method for class 'bats'
sw_tidy_decomp(x, timetk_idx = FALSE, rename_index = "index", ...)
```

## Arguments

- x:

  An object of class "bats" or "tbats"

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

  Used with `sw_augment` and `sw_tidy_decomp`. When `TRUE`, uses a
  timetk index (irregular, typically date or datetime) if present.

## Value

**[`sw_tidy()`](https://business-science.github.io/sweep/reference/sw_tidy.md)**
returns one row for each model parameter, with two columns:

- `term`: The various parameters (lambda, alpha, gamma, etc)

- `estimate`: The estimated parameter value

**[`sw_glance()`](https://business-science.github.io/sweep/reference/sw_glance.md)**
returns one row with the columns

- `model.desc`: A description of the model including the three integer
  components (p, d, q) are the AR order, the degree of differencing, and
  the MA order.

- `sigma`: The square root of the estimated residual variance

- `logLik`: The data's log-likelihood under the model

- `AIC`: The Akaike Information Criterion

- `BIC`: The Bayesian Information Criterion (`NA` for bats / tbats)

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

**[`sw_tidy_decomp()`](https://business-science.github.io/sweep/reference/sw_tidy_decomp.md)**
returns a tibble with the following time series attributes:

- `index`: An index is either attempted to be extracted from the model
  or a sequential index is created for plotting purposes

- `observed`: The original time series

- `level`: The level component

- `slope`: The slope component (Not always present)

- `season`: The seasonal component (Not always present)

## See also

[`forecast::bats()`](https://pkg.robjhyndman.com/forecast/reference/bats.html),
[`forecast::tbats()`](https://pkg.robjhyndman.com/forecast/reference/tbats.html)

## Examples

``` r
library(dplyr)
library(forecast)

fit_bats <- WWWusage %>%
    bats()

sw_tidy(fit_bats)
#> # A tibble: 7 × 2
#>   term              estimate
#>   <chr>                <dbl>
#> 1 lambda               1.000
#> 2 alpha                1.52 
#> 3 beta                NA    
#> 4 damping.parameter   NA    
#> 5 gamma.values        NA    
#> 6 ar.coefficients     NA    
#> 7 ma.coefficients     -0.666
sw_glance(fit_bats)
#> # A tibble: 1 × 12
#>   model.desc sigma logLik   AIC   BIC    ME  RMSE   MAE   MPE  MAPE  MASE   ACF1
#>   <chr>      <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1 BATS(1, {…  3.47   709.  727.  732. 0.217  3.47  2.62 0.261  2.13 0.579 0.0445
sw_augment(fit_bats)
#> # A tibble: 100 × 4
#>    index .actual .fitted  .resid
#>    <int>   <dbl>   <dbl>   <dbl>
#>  1     1      88   101.  -12.7  
#>  2     2      84    75.6   8.38 
#>  3     3      85    85.5  -0.453
#>  4     4      85    84.1   0.867
#>  5     5      84    84.8  -0.818
#>  6     6      85    82.7   2.30 
#>  7     7      83    86.3  -3.29 
#>  8     8      85    80.2   4.78 
#>  9     9      88    88.1  -0.125
#> 10    10      89    89.3  -0.284
#> # ℹ 90 more rows
```
