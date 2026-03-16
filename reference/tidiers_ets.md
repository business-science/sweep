# Tidying methods for ETS (Error, Trend, Seasonal) exponential smoothing modeling of time series

Tidying methods for ETS (Error, Trend, Seasonal) exponential smoothing
modeling of time series

## Usage

``` r
# S3 method for class 'ets'
sw_tidy(x, ...)

# S3 method for class 'ets'
sw_glance(x, ...)

# S3 method for class 'ets'
sw_augment(x, data = NULL, timetk_idx = FALSE, rename_index = "index", ...)

# S3 method for class 'ets'
sw_tidy_decomp(x, timetk_idx = FALSE, rename_index = "index", ...)
```

## Arguments

- x:

  An object of class "ets"

- ...:

  Not used.

- data:

  Used with `sw_augment` only. `NULL` by default which simply returns
  augmented columns only. User can supply the original data, which
  returns the data + augmented columns.

- timetk_idx:

  Used with `sw_augment` and `sw_tidy_decomp`. When `TRUE`, uses a
  timetk index (irregular, typically date or datetime) if present.

- rename_index:

  Used with `sw_augment` and `sw_tidy_decomp`. A string representing the
  name of the index generated.

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

**[`sw_tidy_decomp()`](https://business-science.github.io/sweep/reference/sw_tidy_decomp.md)**
returns a tibble with the following time series attributes:

- `index`: An index is either attempted to be extracted from the model
  or a sequential index is created for plotting purposes

- `observed`: The original time series

- `level`: The level component

- `slope`: The slope component (Not always present)

- `season`: The seasonal component (Not always present)

## See also

[`forecast::ets()`](https://pkg.robjhyndman.com/forecast/reference/ets.html)

## Examples

``` r
library(dplyr)
library(forecast)

fit_ets <- WWWusage %>%
    ets()

sw_tidy(fit_ets)
#> # A tibble: 5 × 2
#>   term  estimate
#>   <chr>    <dbl>
#> 1 alpha   1.000 
#> 2 beta    0.997 
#> 3 phi     0.815 
#> 4 l      90.4   
#> 5 b      -0.0173
sw_glance(fit_ets)
#> # A tibble: 1 × 12
#>   model.desc  sigma logLik   AIC   BIC    ME  RMSE   MAE   MPE  MAPE  MASE  ACF1
#>   <chr>       <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 ETS(A,Ad,N)  3.50  -353.  718.  733. 0.224  3.41  2.76 0.263  2.16 0.610 0.231
sw_augment(fit_ets)
#> # A tibble: 100 × 4
#>    index .actual .fitted .resid
#>    <int>   <dbl>   <dbl>  <dbl>
#>  1     1      88    90.3 -2.34 
#>  2     2      84    86.1 -2.09 
#>  3     3      85    80.7  4.25 
#>  4     4      85    85.8 -0.803
#>  5     5      84    85.0 -1.00 
#>  6     6      85    83.2  1.81 
#>  7     7      83    85.8 -2.81 
#>  8     8      85    81.4  3.62 
#>  9     9      88    86.6  1.38 
#> 10    10      89    90.4 -1.44 
#> # ℹ 90 more rows
sw_tidy_decomp(fit_ets)
#> # A tibble: 101 × 4
#>    index observed level    slope
#>    <dbl>    <dbl> <dbl>    <dbl>
#>  1     0       NA  90.4 -0.0173 
#>  2     1       88  88.0 -2.34   
#>  3     2       84  84.0 -3.99   
#>  4     3       85  85.0  0.986  
#>  5     4       85  85.0  0.00312
#>  6     5       84  84.0 -0.997  
#>  7     6       85  85.0  0.994  
#>  8     7       83  83.0 -1.99   
#>  9     8       85  85.0  1.99   
#> 10     9       88  88.0  3.00   
#> # ℹ 91 more rows
```
