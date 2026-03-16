# Tidying methods for Nural Network Time Series models

These methods tidy the coefficients of NNETAR models of univariate time
series.

## Usage

``` r
# S3 method for class 'nnetar'
sw_tidy(x, ...)

# S3 method for class 'nnetar'
sw_glance(x, ...)

# S3 method for class 'nnetar'
sw_augment(x, data = NULL, timetk_idx = FALSE, rename_index = "index", ...)
```

## Arguments

- x:

  An object of class "nnetar"

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

- `term`: The smoothing parameters (alpha, gamma) and the initial states
  (l, s0 through s10)

- `estimate`: The estimated parameter value

**[`sw_glance()`](https://business-science.github.io/sweep/reference/sw_glance.md)**
returns one row with the columns

- `model.desc`: A description of the model including the three integer
  components (p, d, q) are the AR order, the degree of differencing, and
  the MA order.

- `sigma`: The square root of the estimated residual variance

- `logLik`: The data's log-likelihood under the model (`NA`)

- `AIC`: The Akaike Information Criterion (`NA`)

- `BIC`: The Bayesian Information Criterion (`NA`)

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

[`forecast::nnetar()`](https://pkg.robjhyndman.com/forecast/reference/nnetar.html)

## Examples

``` r
library(dplyr)
library(forecast)

fit_nnetar <- lynx %>%
    nnetar()

sw_tidy(fit_nnetar)
#> # A tibble: 4 × 2
#>   term  estimate
#>   <chr>    <dbl>
#> 1 m            1
#> 2 p            8
#> 3 P            0
#> 4 size         4
sw_glance(fit_nnetar)
#> # A tibble: 1 × 12
#>   model.desc sigma logLik AIC   BIC      ME  RMSE   MAE   MPE  MAPE  MASE
#>   <chr>      <dbl> <lgl>  <lgl> <lgl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 NNAR(8,4)   304. NA     NA    NA    0.226  304.  219. -36.8  50.7 0.263
#> # ℹ 1 more variable: ACF1 <dbl>
sw_augment(fit_nnetar)
#> # A tibble: 114 × 4
#>    index .actual .fitted .resid
#>    <dbl>   <dbl>   <dbl>  <dbl>
#>  1  1821     269     NA    NA  
#>  2  1822     321     NA    NA  
#>  3  1823     585     NA    NA  
#>  4  1824     871     NA    NA  
#>  5  1825    1475     NA    NA  
#>  6  1826    2821     NA    NA  
#>  7  1827    3928     NA    NA  
#>  8  1828    5943     NA    NA  
#>  9  1829    4950   4488.  462. 
#> 10  1830    2577   2589.  -12.0
#> # ℹ 104 more rows
```
