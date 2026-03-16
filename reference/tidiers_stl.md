# Tidying methods for STL (Seasonal, Trend, Level) decomposition of time series

Tidying methods for STL (Seasonal, Trend, Level) decomposition of time
series

## Usage

``` r
# S3 method for class 'stl'
sw_tidy(x, ...)

# S3 method for class 'stl'
sw_tidy_decomp(x, timetk_idx = FALSE, rename_index = "index", ...)

# S3 method for class 'stlm'
sw_tidy_decomp(x, timetk_idx = FALSE, rename_index = "index", ...)

# S3 method for class 'stlm'
sw_glance(x, ...)

# S3 method for class 'stlm'
sw_augment(x, data = NULL, rename_index = "index", timetk_idx = FALSE, ...)
```

## Arguments

- x:

  An object of class "stl"

- ...:

  Not used.

- timetk_idx:

  Used with `sw_tidy_decomp`. When `TRUE`, uses a timetk index
  (irregular, typically date or datetime) if present.

- rename_index:

  Used with `sw_tidy_decomp`. A string representing the name of the
  index generated.

- data:

  Used with `sw_augment` only.

## Value

**[`sw_tidy()`](https://business-science.github.io/sweep/reference/sw_tidy.md)**
wraps
[`sw_tidy_decomp()`](https://business-science.github.io/sweep/reference/sw_tidy_decomp.md)

**[`sw_tidy_decomp()`](https://business-science.github.io/sweep/reference/sw_tidy_decomp.md)**
returns a tibble with the following time series attributes:

- `index`: An index is either attempted to be extracted from the model
  or a sequential index is created for plotting purposes

- `season`: The seasonal component

- `trend`: The trend component

- `remainder`: observed - (season + trend)

- `seasadj`: observed - season (or trend + remainder)

**[`sw_glance()`](https://business-science.github.io/sweep/reference/sw_glance.md)**
returns the underlying ETS or ARIMA model's
[`sw_glance()`](https://business-science.github.io/sweep/reference/sw_glance.md)
results one row with the columns

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

[`stats::stl()`](https://rdrr.io/r/stats/stl.html)

## Examples

``` r
library(dplyr)
library(forecast)
library(sweep)

fit_stl <- USAccDeaths %>%
    stl(s.window = "periodic")

sw_tidy_decomp(fit_stl)
#> # A tibble: 72 × 6
#>    index     observed  season trend remainder seasadj
#>    <yearmon>    <dbl>   <dbl> <dbl>     <dbl>   <dbl>
#>  1 Jan 1973      9007  -820.  9935.    -108.    9827.
#>  2 Feb 1973      8106 -1559.  9881.    -216.    9665.
#>  3 Mar 1973      8928  -760.  9827.    -139.    9688.
#>  4 Apr 1973      9137  -530.  9766.     -98.2   9667.
#>  5 May 1973     10017   335.  9704.     -22.0   9682.
#>  6 Jun 1973     10826   815.  9637.     374.   10011.
#>  7 Jul 1973     11317  1682.  9569.      65.9   9635.
#>  8 Aug 1973     10744   982.  9500.     262.    9762.
#>  9 Sep 1973      9713   -62.8 9431.     345.    9776.
#> 10 Oct 1973      9938   232.  9343.     363.    9706.
#> # ℹ 62 more rows
```
