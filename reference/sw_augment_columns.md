# Augments data

Augments data

## Usage

``` r
sw_augment_columns(ret, data, rename_index, timetk_idx = FALSE)
```

## Arguments

- ret:

  An object of class tibble

- data:

  Any time series data that is to be augmented

- rename_index:

  A variable indicating the index name to be used in the tibble returned

- timetk_idx:

  Uses the timetk index (irregular time index) if present.
