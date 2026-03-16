# Coerces decomposed time-series objects to tibble format.

Coerces decomposed time-series objects to tibble format.

## Usage

``` r
sw_tidy_decomp(x, timetk_idx = FALSE, rename_index = "index", ...)
```

## Arguments

- x:

  A time-series object of class `stl`, `ets`, `decomposed.ts`,
  `HoltWinters`, `bats` or `tbats`.

- timetk_idx:

  When `TRUE`, uses a timetk index (irregular, typically date or
  datetime) if present.

- rename_index:

  Enables the index column to be renamed.

- ...:

  Not used.

## Value

Returns a `tibble` object.

## Details

`sw_tidy_decomp` is designed to coerce time-series objects with
decompositions to `tibble` objects.

A regularized time index is always constructed. If no time index is
detected, a sequential index is returned as a default. The index column
name can be changed using the `rename_index` argument.

## Examples

``` r
library(dplyr)
library(forecast)
library(sweep)

# Decompose ETS model
USAccDeaths %>%
    ets() %>%
    sw_tidy_decomp()
#> # A tibble: 73 × 4
#>    index     observed level  season
#>    <yearmon>    <dbl> <dbl>   <dbl>
#>  1 Dec 1972        NA 9248.   -51.3
#>  2 Jan 1973      9007 9544.  -738. 
#>  3 Feb 1973      8106 9603. -1538. 
#>  4 Mar 1973      8928 9642.  -740. 
#>  5 Apr 1973      9137 9633.  -490. 
#>  6 May 1973     10017 9679.   307. 
#>  7 Jun 1973     10826 9911.   757. 
#>  8 Jul 1973     11317 9746.  1683. 
#>  9 Aug 1973     10744 9762.   971. 
#> 10 Sep 1973      9713 9805.  -122. 
#> # ℹ 63 more rows

# Decompose STL object
USAccDeaths %>%
    stl(s.window = 'periodic') %>%
    sw_tidy_decomp()
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
