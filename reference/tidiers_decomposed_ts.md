# Tidying methods for decomposed time series

Tidying methods for decomposed time series

## Usage

``` r
# S3 method for class 'decomposed.ts'
sw_tidy_decomp(x, timetk_idx = FALSE, rename_index = "index", ...)
```

## Arguments

- x:

  An object of class "decomposed.ts"

- timetk_idx:

  Used with `sw_augment` and `sw_tidy_decomp`. When `TRUE`, uses a
  timetk index (irregular, typically date or datetime) if present.

- rename_index:

  Used with `sw_augment` and `sw_tidy_decomp`. A string representing the
  name of the index generated.

- ...:

  Not used.

## Value

**[`sw_tidy_decomp()`](https://business-science.github.io/sweep/reference/sw_tidy_decomp.md)**
returns a tibble with the following time series attributes:

- `index`: An index is either attempted to be extracted from the model
  or a sequential index is created for plotting purposes

- `season`: The seasonal component

- `trend`: The trend component

- `random`: The error component

- `seasadj`: observed - season

## See also

[`decompose()`](https://rdrr.io/r/stats/decompose.html)

## Examples

``` r
library(dplyr)
library(forecast)

fit_decomposed <- USAccDeaths %>%
    decompose()

sw_tidy_decomp(fit_decomposed)
#> # A tibble: 72 × 6
#>    index     observed season trend random seasadj
#>    <yearmon>    <dbl>  <dbl> <dbl>  <dbl>   <dbl>
#>  1 Jan 1973      9007  -806.   NA    NA     9813.
#>  2 Feb 1973      8106 -1523.   NA    NA     9629.
#>  3 Mar 1973      8928  -741.   NA    NA     9669.
#>  4 Apr 1973      9137  -515.   NA    NA     9652.
#>  5 May 1973     10017   340.   NA    NA     9677.
#>  6 Jun 1973     10826   745.   NA    NA    10081.
#>  7 Jul 1973     11317  1679. 9599.   38.2   9638.
#>  8 Aug 1973     10744   986. 9500.  258.    9758.
#>  9 Sep 1973      9713  -109. 9416.  406.    9822.
#> 10 Oct 1973      9938   264. 9349.  325.    9674.
#> # ℹ 62 more rows
```
