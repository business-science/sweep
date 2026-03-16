# Tidying methods for HoltWinters modeling of time series

These methods tidy `HoltWinters` models of univariate time series.

## Usage

``` r
# S3 method for class 'HoltWinters'
sw_tidy(x, ...)

# S3 method for class 'HoltWinters'
sw_glance(x, ...)

# S3 method for class 'HoltWinters'
sw_augment(x, data = NULL, rename_index = "index", timetk_idx = FALSE, ...)

# S3 method for class 'HoltWinters'
sw_tidy_decomp(x, timetk_idx = FALSE, rename_index = "index", ...)
```

## Arguments

- x:

  An object of class "HoltWinters"

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

- `term`: The various parameters (alpha, beta, gamma, and coefficients)

- `estimate`: The estimated parameter value

**[`sw_glance()`](https://business-science.github.io/sweep/reference/sw_glance.md)**
returns one row with the following columns:

- `model.desc`: A description of the model

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

- `season`: The seasonal component

- `trend`: The trend component

- `remainder`: observed - (season + trend)

- `seasadj`: observed - season (or trend + remainder)

## See also

[`stats::HoltWinters()`](https://rdrr.io/r/stats/HoltWinters.html)

## Examples

``` r
library(dplyr)
library(forecast)

fit_hw <- USAccDeaths %>%
    stats::HoltWinters()

sw_tidy(fit_hw)
#> # A tibble: 17 × 2
#>    term    estimate
#>    <chr>      <dbl>
#>  1 alpha     0.738 
#>  2 beta      0.0223
#>  3 gamma     1     
#>  4 a      8799.    
#>  5 b       -22.7   
#>  6 s1     -802.    
#>  7 s2    -1740.    
#>  8 s3     -960.    
#>  9 s4     -594.    
#> 10 s5      259.    
#> 11 s6      674.    
#> 12 s7     1771.    
#> 13 s8     1031.    
#> 14 s9      211.    
#> 15 s10     549.    
#> 16 s11     128.    
#> 17 s12     441.    
sw_glance(fit_hw)
#> # A tibble: 1 × 12
#>   model.desc sigma logLik AIC   BIC      ME  RMSE   MAE   MPE  MAPE  MASE   ACF1
#>   <chr>      <dbl> <lgl>  <lgl> <lgl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1 HoltWinte… 2939. NA     NA    NA     61.4  379.  274. 0.727  3.21 0.626 0.0569
sw_augment(fit_hw)
#> # A tibble: 72 × 4
#>    index     .actual .fitted .resid
#>    <yearmon>   <dbl>   <dbl>  <dbl>
#>  1 Jan 1973     9007      NA     NA
#>  2 Feb 1973     8106      NA     NA
#>  3 Mar 1973     8928      NA     NA
#>  4 Apr 1973     9137      NA     NA
#>  5 May 1973    10017      NA     NA
#>  6 Jun 1973    10826      NA     NA
#>  7 Jul 1973    11317      NA     NA
#>  8 Aug 1973    10744      NA     NA
#>  9 Sep 1973     9713      NA     NA
#> 10 Oct 1973     9938      NA     NA
#> # ℹ 62 more rows
sw_tidy_decomp(fit_hw)
#> # A tibble: 72 × 6
#>    index     observed season trend remainder seasadj
#>    <yearmon>    <dbl>  <dbl> <dbl>     <dbl>   <dbl>
#>  1 Jan 1973      9007     NA    NA        NA      NA
#>  2 Feb 1973      8106     NA    NA        NA      NA
#>  3 Mar 1973      8928     NA    NA        NA      NA
#>  4 Apr 1973      9137     NA    NA        NA      NA
#>  5 May 1973     10017     NA    NA        NA      NA
#>  6 Jun 1973     10826     NA    NA        NA      NA
#>  7 Jul 1973     11317     NA    NA        NA      NA
#>  8 Aug 1973     10744     NA    NA        NA      NA
#>  9 Sep 1973      9713     NA    NA        NA      NA
#> 10 Oct 1973      9938     NA    NA        NA      NA
#> # ℹ 62 more rows
```
